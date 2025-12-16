# ☕ Coffee Experiment - Quick Start Guide

## ✅ What's Already Done

Everything is set up! Here's what I've prepared for you:

### Dataset ✓
- Downloaded Coffee Quality Database (1,084 samples)
- Preprocessed 11 continuous sensory attributes
- Created `coffee.csv` and `coffee_meta.csv`

### Scenes ✓
- Generated 20,000 training scenes
- Generated 2,000 test scenes
- All in `datasets/coffee/scenes/`

### Configuration ✓
- Created `run.fcg` for FCG-Editor
- Created `monitors.fcg`
- Path configured for your system

---

## 🚀 To Run the Experiment (3 Steps)

### Step 1: Open FCG-Editor

1. Launch FCG-Editor application
2. Open `monitors.fcg` → Click "Evaluate File"
3. Open `run.fcg`

### Step 2: Update Path (Line 17 in run.fcg)

**Current path** (should work if files are in place):
```lisp
(setf cl-user:*babel-corpora* #P"C:\\Users\\yboun\\Downloads\\grounded_naming_game\\grounded_naming_game 2\\code-coffee\\datasets\\")
```

**If you moved the folder**, update to your actual location.

### Step 3: Run!

In FCG-Editor, execute line by line:

```lisp
;; 1. Setup
(setup-experiment "coffee")
(setup-train)

;; 2. Train (100K interactions, ~1-3 hours)
(loop for i from 1 to 100000
      do (run-interaction *experiment*))

;; 3. Test
(notify reset-monitors)
(setup-test)
(loop for i from 1 to 10000
      do (run-interaction *experiment*))

;; 4. View results
(display-lexicon (first (agents *experiment*)) :weight-threshold 0.1 :sort t)
```

---

## 📊 Where to Find Results

**Results location**: `output/experiment/coffee/logging/`

Files generated:
- `communicative-success.csv` - Success rate over time
- `lexicon-coherence.csv` - Agreement between agents
- `unique-form-usage.csv` - Vocabulary size

---

## 📝 Writing Your Report

Use the template: `REPORT_TEMPLATE.md`

**Fill in these sections with your actual results:**
1. Section 3.1 - Insert your metrics from CSV files
2. Section 3.2 - Copy concepts from `display-lexicon` output
3. Section 4 - Insert test set performance numbers

**Submission**: ZIP with:
- `report.pdf` (max 2 pages)
- `preprocess_coffee.py`

---

## ⚠️ Troubleshooting

**GraphViz error?**
→ Ignore it - experiment still works, just no visualizations

**Path not found?**
→ Check line 17 in run.fcg uses `\\` on Windows

**Slow?**
→ Normal: 100K interactions = 1-3 hours

---

## 📚 Full Documentation

See `README.md` for complete details on:
- Dataset description
- All attributes explained
- Step-by-step instructions
- Expected results
- Report writing tips

---

## 🎯 Expected Results Preview

You should see approximately:
- **Communicative Success**: 85-95%
- **Lexicon Coherence**: 80-90%
- **Number of Concepts**: 8-15
- **Test Performance**: Similar to training

**Possible concepts that might emerge:**
- "Balanced" coffees (high balance, moderate all)
- "Acidic/bright" (high acidity, aroma)
- "Full-bodied" (high body, flavor)
- "Sweet/clean" (high sweetness, clean-cup)

---

Good luck! The hard work is done - just run it and analyze! ☕🤖
