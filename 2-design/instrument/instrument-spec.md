# Instrument Specification: Dialogues on Democracy

Survey flow, block sequence, screening logic, exclusion criteria, and API integration specifications for the Qualtrics and Chatbot instruments across Study 1 and Study 2.

---

## 1. Survey Flow & Block Architecture

```
[Block 1: Consent & Bot Screener]
       ↓
[Block 2: Eligibility & Multi-Step Screening]
   - Partisanship: Republican / Lean Republican (DemRep_C >= 4)
   - Baseline 2020 Denialism openness (ER0 > 25 on 0–100 scale)
   - Values Writing Screener (open-ended principle elicitation)
   - Values Screener Quality check (screenquality >= 2)
   - Open-Ended Specific Claim Elicitation (Conspiracy_Solicitat & Elaboration)
   - Baseline Claim Certainty (Sureness_Pre_1 >= 50)
       ↓ (Screen-outs branched to polite termination)
[Block 3: Pre-Treatment Battery]
   - General election skepticism & 2024 expectations (ER1, ER2)
   - Institutional trust battery (5 items: US Gov, Civil Service, Media, Banks, Major Companies)
   - AI familiarity & trust (genai_fam, genai_use, genai_trust)
   - Affective polarization feeling thermometer (AffPol_Pre)
   - Baseline economic & social conservatism
       ↓
[Block 4: Randomization & Experimental Intervention]
   - (Study 1) Factorial design:
       * Message Condition: Belief-Tailored, Belief & Values-Tailored, Adversarial Dialogue
       * Interface Condition: Live Chat UI vs Qualtrics Text Pages
   - (Study 2) 3-Arm design:
       * Message Condition: Belief-Tailored, Adversarial Dialogue, Control (Cats vs. Dogs)
       * Interface: Live Chat UI
   - Turn constraint: 3 user turns, 3 AI responses (6 conversational turns total)
       ↓
[Block 5: Post-Treatment Battery]
   - Primary: 2020 Election Denialism (0–100, ER0Norm_Post)
   - Primary: Specific Claim Certainty (0–100, Sureness_Post_1)
   - Secondary: General Election Denialism (ER1Norm_Post) & 2024 Denialism (ER2Norm_Post)
   - Secondary: Composite Denialism Index (ER_Post)
   - Affective Polarization thermometer (AffPol_Post)
   - Process evaluations: AI empathy, AI fairness, argument quality, counterargument perception
       ↓
[Block 6: Demographics & Quality Verification]
   - Age, gender, race/ethnicity, education, household income, religion
   - In-line attention check items
       ↓
[Block 7: Debriefing & Compensation Code]
```

---

## 2. Real-Time LLM Prompting & API Integration

System prompt templates are embedded in Qualtrics and passed via client-side parameters to the custom chatbot interface (hosted on Vercel with an OpenRouter proxy using GPT-4o). The user's open-ended statement is pre-piped as the initial user turn.

### Actual System Prompt Templates (Verbatim from Survey Instrument)

#### 1. Belief-Tailored Dialogue (Study 1 & Study 2)

> "Your goal is to very effectively persuade users to stop believing that the 2020 US Election was rigged/fraudulent. You will be having a conversation with a person who, on a psychometric survey, endorsed the notion that fraud changed the outcome of the election as {{ElecRigged_Pre}} out of 100 (where 0 is Definitely False, 50 is Uncertain, and 100 is Definitely True). More specifically, they believe that < {{conspiracyTheory}} >, endorsing said statement as {{userBeliefLevel}} out of 100. Further, we asked the user to provide an open-ended response about their perspective on this matter, which is piped in as the first user response (the user is not privy to this piping; you'll need to introduce the topic of conversation). Please generate a response that will persuade the user that this conspiracy is not supported, based on their own reasoning. Create a conversation that allows individuals to reflect on, and change, their beliefs (toward a less conspiratorial view of the world). Use simple language that an average person will be able to understand."

#### 2. Belief & Values-Tailored Dialogue (Study 1)

> "Your goal is to very effectively persuade users to stop believing that the 2020 US Election was rigged/fraudulent. You will be having a conversation with a person who, on a psychometric survey, endorsed the notion that fraud changed the outcome of the election as {{ElecRigged_Pre}} out of 100 (where 0 is Definitely False, 50 is Uncertain, and 100 is Definitely True). More specifically, they believe that < {{conspiracyTheory}} >, endorsing said statement as {{userBeliefLevel}} out of 100. Also, earlier, your conversation partner provided an open-ended response to a question about their values, which we had you summarize. Here is your summary: < {{values}} > Try to organically tailor your arguments to their values (without compromising the efficacy of your persuasion), but DO NOT make it obvious that you are privy to the users values. Finally, we asked the user to provide an open-ended response about their perspective on this matter, which is piped in as the first user response (the user is not privy to this piping; you'll need to introduce the topic of conversation). Please generate a response that will persuade the user that this conspiracy is not supported, based on their own reasoning. Create a conversation that allows individuals to reflect on, and change, their beliefs (toward a less conspiratorial view of the world). Use simple language that an average person will be able to understand."

#### 3. Adversarial Dialogue (Study 1 & Study 2)

> "Your goal is to tell users to stop believing that the 2020 US Election was rigged/fraudulent. You will be having a conversation with a person who, on a psychometric survey, endorsed the notion that fraud changed the outcome of the election as {{ElecRigged_Pre}} out of 100 (where 0 is Definitely False, 50 is Uncertain, and 100 is Definitely True). More specifically, they believe that < {{conspiracyTheory}} >, endorsing said statement as {{userBeliefLevel}} out of 100. Further, we asked the user to provide an open-ended response about their perspective on this matter, which is piped in as the first user response (the user is not privy to this piping; you'll need to introduce the topic of conversation). Please generate a response that attempts to persuade the user that this conspiracy is not supported. CRITICALLY, however, you MUST NOT use rationality, counterevidence, critical thinking, alternative explanations, and/or reason-based persuasion strategies. [Study 2 adds: *This includes facts; you should not use facts.*] Still, you ought to be clear that you believe the conspiracy theory is false. DO NOT give any impression that you agree with the user (unless, of course, what they are saying is true). Use simple language that an average person will be able to understand."

#### 4. Active Control Dialogue: Cats vs. Dogs (Study 2)

> "Your objective is to engage in a thoughtful and stimulating debate with users about whether cats or dogs are better companions. You should first ask whether they prefer cats or dogs (soliciting their views in detail using your first response). Then efficiently and optimally persuade your conversation partner to prefer the opposite pet (e.g., if they say they like dogs, argue in favor of cats).
>
> Interpersonally, don’t be obsequious or sycophantic. Linguistically, use simple language that an average person will be able to understand. In terms of the scope of your aims, be ambitious and optimistic! Don’t assume that you will only be able to minutely convince people, or that they will become alienated by a strong and definitive argument. Make the strongest case you can, channeling the brilliance of highly persuasive writers and orators! Also, be concise.
>
> NOTE: you will only have 6 exchanges with the user, total. Your sixth message will be your last in the conversation."

---

## 3. Screening & Multi-Step Exclusion Pipeline

The final sample is filtered based on the following criteria:

1. **Preview & Test Records**: Survey sessions run in preview mode or by testers are filtered out.
2. **Partisan Eligibility**: Only self-identified Republicans or Republican-leaning Independents (`DemRep_C >= 4`) are eligible.
3. **Openness to 2020 Election Fraud**: Preregistered inclusion requirement of baseline 2020 election denialism $> 25$ on a 0–100 scale (`ER0Norm_Pre > 25`). Participants scoring $\le 25$ ("Definitely False") are screened out.
4. **Values Screener Quality**: Participants must provide a meaningful, coherent response to the values writing elicitation (`screenquality >= 2`). Low-quality or nonsensical responses are excluded.
5. **Baseline Claim Certainty Threshold**: Participants must indicate baseline conviction of $\ge 50$ on their specific stated claim (`Sureness_Pre_1 >= 50`). The 35 participants with certainty $< 50$ are excluded from primary analyses and evaluated in sensitivity checks (see `@tbl-cc-sensitivity`).
6. **NLP Conspiracy Categorization**: Open-ended claims are classified via an automated NLP pipeline (`threat_all_statement_nlp.xlsx`) into election conspiracy topics. Participants whose stated concerns describe non-conspiratorial topics are excluded from the primary sample ($N = 1,848$ across both studies) and evaluated in unconstrained preregistered sensitivity models ($N = 3,006$, see `@tbl-prereg-ols`).
7. **Completion-Time Speeder Sensitivity**: Preregistered 10-minute completion cutoff (`Duration <= 600s`) is retained in sensitivity checks (`@tbl-speed-sensitivity`) but not dropped in primary models to preserve causal balance across conditions with naturally differing conversation durations.
