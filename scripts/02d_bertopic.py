#!/usr/bin/env python3
"""
Run BERTopic on df_both$userResponse_1 from clean_dfs.RData and export
Top-N topics + a bar chart with % and n.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
from typing import Iterable, Tuple

import pandas as pd

from bertopic import BERTopic
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS
import matplotlib.pyplot as plt


# scripts/ -> 4-analysis/data/clean_dfs.RData (relative to this file, so it works on any clone)
DEFAULT_RDATA = str(Path(__file__).resolve().parent.parent / "data" / "clean_dfs.RData")
DEFAULT_OUT_DIR = Path(__file__).resolve().parent

NEGATION_TERMS = {
    "no",
    "not",
    "nor",
    "never",
    "none",
    "nobody",
    "nothing",
    "neither",
    "nowhere",
    "cannot",
    "can't",
    "dont",
    "don't",
    "won't",
    "wont",
    "isn't",
    "isnt",
    "aren't",
    "arent",
    "wasn't",
    "wasnt",
    "weren't",
    "werent",
    "didn't",
    "didnt",
    "doesn't",
    "doesnt",
    "shouldn't",
    "shouldnt",
    "wouldn't",
    "wouldnt",
    "couldn't",
    "couldnt",
    "mustn't",
    "mustnt",
}

TOKEN_RE = re.compile(r"[a-z0-9']+")


def export_user_response_via_rscript(rdata_path: Path, csv_path: Path) -> None:
    rdata_str = rdata_path.as_posix()
    csv_str = csv_path.as_posix()
    r_code = (
        f"load('{rdata_str}');"
        "if (!exists('df_both')) stop('df_both not found');"
        "df_tmp <- data.frame(userResponse_1 = df_both$userResponse_1, "
        "stringsAsFactors = FALSE);"
        f"write.csv(df_tmp, '{csv_str}', row.names=FALSE, fileEncoding='UTF-8')"
    )
    subprocess.run(["Rscript", "-e", r_code], check=True)


def load_user_response_from_rdata(rdata_path: Path) -> pd.Series:
    with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as tmp:
        temp_path = Path(tmp.name)
    try:
        export_user_response_via_rscript(rdata_path, temp_path)
        df = pd.read_csv(temp_path)
    except FileNotFoundError as exc:
        raise SystemExit(
            "Rscript not found. Install R or provide --csv with userResponse_1."
        ) from exc
    finally:
        if temp_path.exists():
            temp_path.unlink(missing_ok=True)
    if "userResponse_1" not in df.columns:
        raise SystemExit("Column userResponse_1 not found in exported data.")
    return df["userResponse_1"]


def load_user_response_from_csv(csv_path: Path) -> pd.Series:
    df = pd.read_csv(csv_path)
    if "userResponse_1" not in df.columns:
        raise SystemExit("Column userResponse_1 not found in CSV.")
    return df["userResponse_1"]


def get_stopwords() -> set[str]:
    try:
        import nltk
        from nltk.corpus import stopwords

        nltk.data.find("corpora/stopwords")
        stop_words = set(stopwords.words("english"))
    except Exception:
        stop_words = set(ENGLISH_STOP_WORDS)
    stop_words = {w for w in stop_words if w not in NEGATION_TERMS}
    return stop_words


def get_lemmatizer():
    try:
        import nltk
        from nltk.stem import WordNetLemmatizer

        nltk.data.find("corpora/wordnet")
        return WordNetLemmatizer()
    except Exception:
        return None


def get_stemmer():
    try:
        from nltk.stem import PorterStemmer

        return PorterStemmer()
    except Exception:
        return None


def handle_negation(tokens: list[str], window: int = 3) -> list[str]:
    output = []
    remaining = 0
    for token in tokens:
        if token in NEGATION_TERMS:
            remaining = window
            output.append(token)
            continue
        if remaining > 0:
            output.append(f"not_{token}")
            remaining -= 1
        else:
            output.append(token)
    return output


def clean_texts(texts: list[str]) -> list[str]:
    stop_words = get_stopwords()
    lemmatizer = get_lemmatizer()
    stemmer = get_stemmer()
    cleaned = []
    for text in texts:
        lowered = str(text).lower()
        lowered = lowered.replace("n't", " not")
        tokens = TOKEN_RE.findall(lowered)
        tokens = handle_negation(tokens)
        tokens = [t for t in tokens if t not in stop_words and len(t) > 1]
        if lemmatizer:
            tokens = [lemmatizer.lemmatize(t) for t in tokens]
        if stemmer:
            tokens = [stemmer.stem(t) for t in tokens]
        cleaned.append(" ".join(tokens))
    return cleaned


def build_topic_label(row: pd.Series) -> str:
    if "Name" in row and isinstance(row["Name"], str) and row["Name"].strip():
        return row["Name"].strip()
    if "Representation" in row:
        rep = row["Representation"]
        if isinstance(rep, (list, tuple)):
            rep = ", ".join([str(item) for item in rep if str(item).strip()])
        if isinstance(rep, str) and rep.strip():
            return rep.strip()
    return f"Topic {row['Topic']}"

def clean_openai_label(raw: str, max_words: int = 6) -> str:
    if not raw:
        return ""
    label = raw.strip().strip("\"").strip("'")
    match = re.search(r"\"label\"\\s*:\\s*\"([^\"]+)\"", label)
    if match:
        label = match.group(1)
    label = label.splitlines()[0]
    label = label.split(";")[0]
    label = re.sub(r"^\\s*\\d+\\s*[\\).:-]\\s*", "", label)
    label = re.sub(r"\\s+", " ", label).strip()
    words = label.split()
    if len(words) > max_words:
        label = " ".join(words[:max_words])
    return label


def safe_openai_prompt(keywords: str, min_topics: int, max_topics: int) -> str:
    return (
        "You are labeling themes about concerns regarding the integrity of the 2020 US election. "
        f"We want roughly {min_topics}-{max_topics} distinct concerns overall, so be specific. "
        "Provide one concise, nuanced theme label (2-6 words). "
        "Do not include numbering or multiple labels. "
        "Return JSON only in the form: {\"label\": \"...\"}.\n\n"
        f"Keywords: {keywords}"
    )


def label_topics_with_openai(
    topic_model: BERTopic,
    topic_ids: list[int],
    model: str,
    min_topics: int,
    max_topics: int,
    max_words: int = 10,
) -> dict[int, str]:
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key:
        raise SystemExit("OPENAI_API_KEY is not set.")
    try:
        from openai import OpenAI

        client = OpenAI(api_key=api_key)

        def call_api(prompt: str) -> str:
            response = client.responses.create(model=model, input=prompt)
            return response.output_text
    except Exception:
        import openai

        openai.api_key = api_key

        def call_api(prompt: str) -> str:
            response = openai.ChatCompletion.create(
                model=model,
                messages=[{"role": "user", "content": prompt}],
            )
            return response["choices"][0]["message"]["content"]

    labels: dict[int, str] = {}
    for topic_id in topic_ids:
        words = topic_model.get_topic(topic_id)
        if not words:
            labels[topic_id] = f"Topic {topic_id}"
            continue
        keywords = ", ".join([w for w, _ in words[:max_words]])
        prompt = safe_openai_prompt(keywords, min_topics, max_topics)
        raw_label = call_api(prompt)
        label = clean_openai_label(raw_label, max_words=6)
        labels[topic_id] = label if label else f"Topic {topic_id}"
        time.sleep(0.25)
    return labels


def fit_topic_model(
    docs: list[str],
    embedding_model: str,
    min_cluster_size: int,
    min_samples: int,
    min_df: int,
) -> Tuple[BERTopic, list[int], int]:
    vectorizer_model = CountVectorizer(ngram_range=(1, 2), min_df=min_df)
    hdbscan_model = HDBSCAN(
        min_cluster_size=min_cluster_size,
        min_samples=min_samples,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )

    topic_model = BERTopic(
        embedding_model=embedding_model,
        vectorizer_model=vectorizer_model,
        hdbscan_model=hdbscan_model,
        verbose=True,
    )

    topics, _ = topic_model.fit_transform(docs)
    topic_info = topic_model.get_topic_info()
    topic_count = len(topic_info[topic_info["Topic"] != -1])
    return topic_model, topics, topic_count


def reduce_topics_if_needed(
    topic_model: BERTopic,
    docs: list[str],
    topics: list[int],
    max_topics: int,
) -> tuple[BERTopic, list[int]]:
    try:
        reduced = topic_model.reduce_topics(docs, topics, nr_topics=max_topics)
    except TypeError:
        reduced = topic_model.reduce_topics(docs, nr_topics=max_topics)

    if isinstance(reduced, BERTopic):
        topic_model = reduced
        topics = topic_model.topics_
    elif isinstance(reduced, tuple):
        first = reduced[0] if reduced else None
        if isinstance(first, BERTopic):
            topic_model = first
            if len(reduced) > 1 and isinstance(reduced[1], (list, tuple)):
                topics = list(reduced[1])
            else:
                topics = topic_model.topics_
        else:
            topics = list(first) if first is not None else topics
    else:
        topics = list(reduced) if reduced is not None else topics

    return topic_model, topics
    return reduced


def main() -> int:
    parser = argparse.ArgumentParser(
        description="BERTopic on df_both$userResponse_1 with top-N outputs."
    )
    parser.add_argument(
        "--rdata",
        default=DEFAULT_RDATA,
        help="Path to clean_dfs.RData (default: %(default)s)",
    )
    parser.add_argument(
        "--csv",
        default=None,
        help="Optional CSV with userResponse_1 to skip RData loading.",
    )
    parser.add_argument(
        "--out-dir",
        default=str(DEFAULT_OUT_DIR),
        help="Output directory for CSV/PNG (default: script directory)",
    )
    parser.add_argument(
        "--prefix",
        default="bertopic_userResponse_1_top10",
        help="Output filename prefix (default: %(default)s)",
    )
    parser.add_argument(
        "--top-n",
        type=int,
        default=10,
        help="Number of top topics to export (default: %(default)s)",
    )
    parser.add_argument(
        "--min-topics",
        type=int,
        default=5,
        help="Minimum number of topics to target (default: %(default)s)",
    )
    parser.add_argument(
        "--max-topics",
        type=int,
        default=20,
        help="Maximum number of topics to target (default: %(default)s)",
    )
    parser.add_argument(
        "--min-cluster-size",
        type=int,
        default=10,
        help="HDBSCAN min_cluster_size (default: %(default)s)",
    )
    parser.add_argument(
        "--min-samples",
        type=int,
        default=5,
        help="HDBSCAN min_samples (default: %(default)s)",
    )
    parser.add_argument(
        "--min-df",
        type=int,
        default=1,
        help="Vectorizer min_df (default: %(default)s)",
    )
    parser.add_argument(
        "--embedding-model",
        default="all-MiniLM-L6-v2",
        help="Sentence transformer model (default: %(default)s)",
    )
    parser.add_argument(
        "--no-auto-tune",
        action="store_true",
        help="Disable auto-tuning for the topic count.",
    )
    parser.add_argument(
        "--openai-labels",
        action="store_true",
        help="Use OpenAI API to label topics.",
    )
    parser.add_argument(
        "--openai-model",
        default="gpt-4o-mini",
        help="OpenAI model for labeling (default: %(default)s)",
    )
    args = parser.parse_args()

    out_dir = Path(args.out_dir).expanduser()
    out_dir.mkdir(parents=True, exist_ok=True)

    if args.csv:
        series = load_user_response_from_csv(Path(args.csv).expanduser())
    else:
        rdata_path = Path(args.rdata).expanduser()
        series = load_user_response_from_rdata(rdata_path)

    texts = series[series.notna()].astype(str)
    texts = texts[texts.str.strip() != ""]

    if texts.empty:
        raise SystemExit("No non-empty userResponse_1 values found.")

    docs_clean = clean_texts(texts.tolist())

    param_sets: list[Tuple[int, int, int]] = [
        (args.min_cluster_size, args.min_samples, args.min_df)
    ]
    if not args.no_auto_tune:
        param_sets.extend(
            [
                (max(5, args.min_cluster_size // 2), max(2, args.min_samples // 2), 1),
                (max(3, args.min_cluster_size // 3), 1, 1),
            ]
        )

    seen = set()
    unique_params = []
    for params in param_sets:
        if params not in seen:
            unique_params.append(params)
            seen.add(params)

    best_model = None
    best_topics = None
    best_count = None

    for min_cluster_size, min_samples, min_df in unique_params:
        model, topics, topic_count = fit_topic_model(
            docs_clean,
            embedding_model=args.embedding_model,
            min_cluster_size=min_cluster_size,
            min_samples=min_samples,
            min_df=min_df,
        )
        print(
            f"Topic count {topic_count} with min_cluster_size={min_cluster_size}, "
            f"min_samples={min_samples}, min_df={min_df}"
        )
        best_model, best_topics, best_count = model, topics, topic_count
        if topic_count >= args.min_topics:
            break

    topic_model = best_model
    topics = best_topics

    topic_info = topic_model.get_topic_info()
    topic_count = len(topic_info[topic_info["Topic"] != -1])
    if topic_count > args.max_topics:
        topic_model, topics = reduce_topics_if_needed(
            topic_model, docs_clean, topics, args.max_topics
        )
        topic_info = topic_model.get_topic_info()
        topic_count = len(topic_info[topic_info["Topic"] != -1])

    if topic_count < args.min_topics:
        print(
            f"Warning: only {topic_count} topics found. "
            "Consider lowering --min-cluster-size or --min-samples."
        )

    topic_info = topic_info[topic_info["Topic"] != -1].copy()

    if args.openai_labels:
        topic_ids = topic_info["Topic"].tolist()
        ai_labels = label_topics_with_openai(
            topic_model,
            topic_ids,
            args.openai_model,
            args.min_topics,
            args.max_topics,
        )
        topic_info["topic_label"] = topic_info["Topic"].map(ai_labels)
    else:
        topic_info["topic_label"] = topic_info.apply(build_topic_label, axis=1)

    topic_info["topic_label"] = topic_info["topic_label"].str.slice(0, 60)

    total_topic_n = sum(1 for topic in topics if topic != -1)
    if total_topic_n == 0:
        raise SystemExit("All documents were assigned to the outlier topic (-1).")

    top_n = min(args.top_n, len(topic_info))
    top_topics = (
        topic_info.sort_values("Count", ascending=False)
        .head(top_n)
        .copy()
        .rename(columns={"Topic": "topic_id", "Count": "n"})
    )
    top_topics["percentage"] = top_topics["n"] / total_topic_n * 100

    csv_path = out_dir / f"{args.prefix}.csv"
    top_topics[["topic_id", "topic_label", "n", "percentage"]].to_csv(
        csv_path, index=False
    )

    plot_df = top_topics.sort_values("percentage")
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.barh(plot_df["topic_label"], plot_df["percentage"], color="#080E5B")
    max_pct = plot_df["percentage"].max()
    for idx, (pct, n) in enumerate(zip(plot_df["percentage"], plot_df["n"])):
        ax.text(
            pct + max_pct * 0.01,
            idx,
            f"{pct:.2f}% (n={n})",
            va="center",
            fontsize=9,
        )
    ax.set_xlabel("% of Responses")
    ax.set_title("Top Topics in userResponse_1")
    ax.set_xlim(0, max_pct * 1.15)
    fig.tight_layout()

    png_path = out_dir / f"{args.prefix}.png"
    fig.savefig(png_path, dpi=300)

    print(f"Wrote: {csv_path}")
    print(f"Wrote: {png_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
