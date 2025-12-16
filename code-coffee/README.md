# ☕ Coffee Quality Concept Learning Experiment

This directory contains a complete concept learning experiment using the **Coffee Quality Database** from the Coffee Quality Institute (CQI).

## 📊 Dataset Information

**Dataset**: Coffee Quality Database (Arabica)
**Source**: https://github.com/jldbc/coffee-quality-database
**Original Size**: 1,311 coffee samples from professional tastings
**Final Size**: 1,084 samples (after removing missing values)

### Attributes (11 continuous sensory ratings)

All attributes are professional coffee cupping scores, normalized to [0, 1]:

| Attribute | Description | Original Scale |
|-----------|-------------|----------------|
| **Aroma** | Fragrance/smell intensity | 0-10 |
| **Flavor** | Taste intensity | 0-10 |
| **Aftertaste** | Lingering taste after swallowing | 0-10 |
| **Acidity** | Brightness/liveliness | 0-10 |
| **Body** | Mouthfeel/texture/weight | 0-10 |
| **Balance** | How well flavors work together | 0-10 |
| **Uniformity** | Consistency across cups | 0-10 |
| **Clean-Cup** | Lack of off-flavors/defects | 0-10 |
| **Sweetness** | Natural sweetness perception | 0-10 |
| **Moisture** | Bean moisture content | 0-1 |
| **altitude-mean-meters** | Growing altitude (normalized) | Variable |

## 🎯 Experiment Goals

The experiment investigates:
1. Can agents establish a shared vocabulary for describing coffee sensory profiles?
2. What concepts emerge? (e.g., "balanced", "acidic", "full-bodied", "sweet")
3. Which attributes do agents find most distinctive?
4. Does the emergent vocabulary generalize to unseen coffee samples?

## 📁 Directory Structure

```
code-coffee/
├── README.md (this file)
├── run.fcg (FCG-Editor experiment script)
├── monitors.fcg (monitors configuration)
├── datasets/
│   └── coffee/
│       ├── coffee_meta.csv (attribute metadata)
│       └── scenes/
│           ├── train/ (20,000 training scenes)
│           └── test/ (2,000 test scenes)
└── create-dataset/
    ├── raw/
    │   └── coffee_quality_arabica.csv (original data)
    ├── coffee.csv (processed data)
    ├── coffee_meta.csv (metadata)
    ├── download_coffee_data.py
    ├── preprocess_coffee.py
    └── create-scenes.py
```

## 🚀 Running the Experiment

### Prerequisites

1. **FCG-Editor** installed (download from WebCampus/Canvas)
2. **GraphViz** installed (optional, for visualizations): https://graphviz.org/download/
3. Python environment with pandas (already set up)

### Step 1: Configure Path

1. Open `run.fcg` in FCG-Editor
2. Update line 17 with your actual path:

```lisp
;; WINDOWS:
(setf cl-user:*babel-corpora* #P"C:\\Your\\Path\\To\\code-coffee\\datasets\\")

;; MAC/LINUX:
;; (setf cl-user:*babel-corpora* #P"/Users/your-name/path/to/code-coffee/datasets/")
```

**IMPORTANT**:
- Windows: Use double backslashes `\\`
- End path with `\\` (Windows) or `/` (Mac/Linux)

### Step 2: Load Monitors

1. In FCG-Editor, open `monitors.fcg`
2. Click "Evaluate File" button
3. Return to `run.fcg`

### Step 3: Run Training

Execute these commands in order:

```lisp
;; Setup experiment
(setup-experiment "coffee")

;; Activate learning mode
(setup-train)

;; Run 100,000 interactions (takes 1-3 hours)
(loop for i from 1 to 100000
      do (run-interaction *experiment*))
```

**Progress Output**:
```
. (1000 / 75.2% / 68.5% / 0h 2m 15s)
  ^      ^       ^        ^
  |      |       |        └─ Time elapsed
  |      |       └────────── Lexicon coherence %
  |      └────────────────── Communicative success %
  └───────────────────────── Interaction number
```

### Step 4: Test on Unseen Data

```lisp
;; Reset monitors
(notify reset-monitors)

;; Switch to test mode (no learning)
(setup-test)

;; Run 10,000 test interactions
(loop for i from 1 to 10000
      do (run-interaction *experiment*))
```

### Step 5: Analyze Results

#### View Agent Lexicons:
```lisp
(display-lexicon (first (agents *experiment*)) :weight-threshold 0.1 :sort t)
```

#### Inspect Individual Interactions:
```lisp
;; Enable detailed web interface
(activate-monitor trace-interaction-in-web-interface)

;; Run one interaction
(run-interaction *experiment*)

;; View at: http://localhost:8010

;; Disable when done
(deactivate-monitor trace-interaction-in-web-interface)
```

## 📈 Results Files

Results are saved in: `output/experiment/coffee/logging/`

### Generated Files:

1. **communicative-success.csv**
   - Success rate over time
   - Shows how often agents correctly identify target coffee

2. **lexicon-coherence.csv**
   - Alignment between agents
   - Shows how much agents agree on word meanings

3. **unique-form-usage.csv**
   - Number of distinct word forms used
   - Tracks vocabulary size over time

## 📝 Expected Results

### Training Phase (100,000 interactions):
- **Communicative Success**: Should rise from ~10% → 85-95%
- **Lexicon Coherence**: Should rise from ~0% → 80-90%
- **Vocabulary Size**: Should stabilize around 8-15 concepts

### Possible Emergent Concepts:

Based on the attributes, agents might learn to distinguish:

- **"Balanced coffee"** → High balance, moderate all attributes
- **"Acidic/bright"** → High acidity, high aroma
- **"Full-bodied"** → High body, high flavor
- **"Sweet/smooth"** → High sweetness, high clean-cup, low acidity
- **"Complex"** → High flavor, high aftertaste
- **"Simple/clean"** → High uniformity, high clean-cup
- **"High-altitude"** → High altitude, high acidity

### Test Phase:
- Success rate should remain close to training rate
- Demonstrates generalization to unseen coffee samples

## 📊 Report Writing Guide

Your 2-page report should include:

### 1. Dataset Selection (0.5 pages)
- **Dataset**: Coffee Quality Database from CQI
- **Source**: GitHub repository (professional coffee tastings)
- **Size**: 1,084 samples, 11 continuous attributes
- **Entities**: Arabica coffee samples from various origins
- **Attributes**: Professional cupping scores (aroma, flavor, acidity, body, etc.) + altitude
- **Motivation**:
  - Real-world sensory data
  - Professional expert annotations
  - Rich continuous attribute space
  - Interesting for learning concepts like "balanced", "acidic", "full-bodied"

### 2. Preprocessing Choices (0.3 pages)
- Merged red/white → *(N/A for coffee, different approach)*
- Selected 11 most relevant sensory attributes
- Removed samples with missing values (1311 → 1084)
- Normalized all values to [0,1] range
- Cleaned column names (replaced dots/spaces with dashes)

### 3. Experimental Results (0.7 pages)
- **Communicative Success**: [INSERT FINAL %]
- **Lexicon Coherence**: [INSERT FINAL %]
- **Number of Concepts**: [COUNT FROM display-lexicon]
- **Attributes Captured**: [WHICH CHANNELS - e.g., balance, acidity, body]
- **Interpretation**: Describe what the emergent concepts represent
  - Example: "Concept-1 captures high-acidity, high-aroma coffees"

### 4. Test Set Performance (0.3 pages)
- **Test Success Rate**: [INSERT %]
- **Comparison to Training**: [DISCUSS]
- **Generalization**: How well did concepts transfer to unseen coffees?

### 5. Discussion (0.2 pages)
- Were results as expected?
- Which attributes were most useful for discrimination?
- Limitations (e.g., did agents ignore altitude?)
- Insights into concept formation

## 🔧 Troubleshooting

### Common Issues:

**1. Path not found error**
```
Could not find a 'scenes' subdirectory
```
→ Check path in `run.fcg` line 17 (use `\\` on Windows, ends with `\\`)

**2. GraphViz error**
```
Render program dot is not found
```
→ Install GraphViz or ignore (experiment still works, just no visualizations)

**3. Slow performance**
→ Normal: 100K interactions take 1-3 hours depending on machine

**4. Port 8010 in use**
→ Close other FCG-Editor instances or use different port

## 📚 References

- Coffee Quality Database: https://github.com/jldbc/coffee-quality-database
- Paper: "Emergent Communication in Continuous Worlds" (on Canvas/WebCampus)
- Specialty Coffee Association: https://sca.coffee/research/protocols-best-practices

## ✅ Submission Checklist

Create a ZIP file with:
- [ ] **report.pdf** (max 2 pages)
- [ ] **preprocess_coffee.py** or **coffee.ipynb** (preprocessing script)

**Deadline**: Sunday, December 14 at 23:59

## 🎓 Learning Objectives

This experiment demonstrates:
1. **Emergent communication**: Agents develop shared vocabulary without explicit teaching
2. **Grounded semantics**: Words grounded in continuous sensory perceptions
3. **Concept formation**: Learning categorical distinctions in continuous space
4. **Generalization**: Applying learned concepts to novel instances

Good luck with your coffee concept learning experiment! ☕🤖
