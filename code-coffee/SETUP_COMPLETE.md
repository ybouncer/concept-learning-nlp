# ☕ Coffee Experiment Setup Complete! ✅

## 🎉 Everything is Ready to Run

I've created a complete concept learning experiment using the Coffee Quality Database. All files are prepared and configured for your system.

---

## 📁 What Was Created

### Complete Directory Structure:
```
code-coffee/
├── README.md                    ← Full documentation
├── QUICKSTART.md                ← 3-step run guide
├── REPORT_TEMPLATE.md           ← Pre-filled report (just add your results!)
├── SETUP_COMPLETE.md            ← This file
├── run.fcg                      ← FCG-Editor script (configured for coffee)
├── monitors.fcg                 ← Monitoring configuration
├── datasets/
│   └── coffee/
│       ├── coffee_meta.csv      ← Attribute metadata (11 attributes)
│       └── scenes/
│           ├── train/           ← 20,000 training scenes ✓
│           └── test/            ← 2,000 test scenes ✓
└── create-dataset/
    ├── raw/
    │   └── coffee_quality_arabica.csv  ← Original data
    ├── coffee.csv               ← Processed data (1,084 samples)
    ├── coffee_meta.csv          ← Metadata
    ├── coffee.ipynb             ← Jupyter notebook (YOUR DELIVERABLE!) ⭐
    ├── download_coffee_data.py  ← Download script
    ├── preprocess_coffee.py     ← Preprocessing script
    └── create-scenes.py         ← Scene generation script
```

---

## 📊 Dataset Summary

**Coffee Quality Database**
- **Source**: Coffee Quality Institute (CQI)
- **Total samples**: 1,084 professional coffee tastings
- **Attributes**: 11 continuous sensory scores
- **Training scenes**: 20,000 (with 3-10 coffees each)
- **Test scenes**: 2,000 (with 3-10 coffees each)

### The 11 Attributes:

| # | Attribute | Description | Range |
|---|-----------|-------------|-------|
| 1 | Aroma | Fragrance intensity | 0-10 (normalized to 0-1) |
| 2 | Flavor | Taste intensity | 0-10 (normalized to 0-1) |
| 3 | Aftertaste | Lingering quality | 0-10 (normalized to 0-1) |
| 4 | Acidity | Brightness | 0-10 (normalized to 0-1) |
| 5 | Body | Mouthfeel/texture | 0-10 (normalized to 0-1) |
| 6 | Balance | Harmony | 0-10 (normalized to 0-1) |
| 7 | Uniformity | Consistency | 0-10 (normalized to 0-1) |
| 8 | Clean-Cup | Lack of defects | 0-10 (normalized to 0-1) |
| 9 | Sweetness | Natural sweetness | 0-10 (normalized to 0-1) |
| 10 | Moisture | Bean moisture | % (normalized to 0-1) |
| 11 | altitude-mean-meters | Growing altitude | meters (normalized to 0-1) |

---

## 🚀 How to Run (3 Simple Steps)

### Step 1: Open FCG-Editor
1. Launch the FCG-Editor application
2. File → Open → `monitors.fcg`
3. Click "Evaluate File" button
4. File → Open → `run.fcg`

### Step 2: Verify Path (Line 17)
The path is already configured:
```lisp
(setf cl-user:*babel-corpora* #P"C:\\Users\\yboun\\Downloads\\grounded_naming_game\\grounded_naming_game 2\\code-coffee\\datasets\\")
```

If you moved files, update this path.

### Step 3: Execute Commands
In FCG-Editor, run these commands:

```lisp
;; Setup experiment
(setup-experiment "coffee")
(setup-train)

;; Run 100,000 training interactions (~1-3 hours)
(loop for i from 1 to 100000
      do (run-interaction *experiment*))

;; Test on unseen data
(notify reset-monitors)
(setup-test)
(loop for i from 1 to 10000
      do (run-interaction *experiment*))

;; View emergent concepts
(display-lexicon (first (agents *experiment*)) :weight-threshold 0.1 :sort t)
```

---

## 📈 What to Expect

### During Training:
Progress dots every 100 interactions:
```
. (1000 / 75.2% / 68.5% / 0h 2m 15s)
  ↑      ↑       ↑        ↑
  games  success coherence time
```

### Expected Final Results:
- **Communicative Success**: 85-95%
- **Lexicon Coherence**: 80-90%
- **Vocabulary Size**: 8-15 concepts
- **Test Performance**: Similar to training

### Possible Emergent Concepts:
- **"Balanced"**: High balance, moderate all attributes
- **"Acidic/Bright"**: High acidity, high aroma
- **"Full-bodied"**: High body, high flavor
- **"Sweet/Smooth"**: High sweetness, clean cup
- **"Complex"**: High flavor, high aftertaste
- **"Clean"**: High uniformity, high clean-cup
- **"High-altitude"**: Elevated altitude score

---

## 📂 Where to Find Results

After running, check:
```
code-coffee/output/experiment/coffee/logging/
├── communicative-success.csv    ← Success over time
├── lexicon-coherence.csv        ← Agent alignment
└── unique-form-usage.csv        ← Vocabulary size
```

---

## 📝 Writing Your Report

### Use the Template!
Open `REPORT_TEMPLATE.md` - it's 90% pre-filled!

### What You Need to Add:
1. **Section 3.1**: Insert your actual metrics
2. **Section 3.2**: Copy concepts from `display-lexicon`
3. **Section 4**: Insert test performance numbers
4. **Section 5**: Add your interpretation

### Example - How to Fill Section 3.2:

After running `display-lexicon`, you'll see output like:
```
Form: "wug-42"
  Channels: Acidity(0.85), Aroma(0.78), Body(0.32)
  Weight: 0.92
```

In your report, write:
```markdown
**Concept 1: "wug-42"**
- Attributes: High acidity (0.85), high aroma (0.78), low body (0.32)
- Interpretation: Represents "bright, aromatic, light-bodied" coffees
- Weight: 0.92 (highly entrenched)
```

### Final Submission:
1. Convert `REPORT_TEMPLATE.md` to PDF (max 2 pages)
2. ZIP it with **`coffee.ipynb`** (the Jupyter notebook)
3. Submit to WebCampus/Canvas by **Dec 14, 23:59**

**Deliverables:**
- ✅ `report.pdf` (max 2 pages)
- ✅ `coffee.ipynb` (preprocessing notebook)

---

## 🔧 Troubleshooting

| Problem | Solution |
|---------|----------|
| **GraphViz error** | Ignore - experiment works, just no visualizations |
| **Path not found** | Check line 17 in run.fcg, use `\\` on Windows |
| **Slow performance** | Normal: 100K = 1-3 hours |
| **Port 8010 in use** | Close other FCG-Editor instances |

---

## 📚 Documentation Files

- **README.md**: Complete detailed guide (8 pages)
- **QUICKSTART.md**: Ultra-short 3-step guide
- **REPORT_TEMPLATE.md**: Pre-filled report template
- **SETUP_COMPLETE.md**: This file

---

## ✅ Verification Checklist

Before running, verify:

- [x] Coffee dataset downloaded (1,084 samples)
- [x] Data preprocessed (11 attributes, normalized)
- [x] 20,000 training scenes created
- [x] 2,000 test scenes created
- [x] Metadata file created
- [x] FCG-Editor files configured
- [x] Path in run.fcg set correctly
- [x] Documentation created

**Status**: ✅ READY TO RUN!

---

## 🎓 What This Experiment Shows

This concept learning experiment demonstrates:

1. **Emergent Communication**: Agents develop shared vocabulary without explicit teaching
2. **Grounded Semantics**: Words grounded in continuous sensory perceptions
3. **Concept Formation**: Learning categories in complex continuous space
4. **Generalization**: Applying learned concepts to novel coffee samples

---

## 🎯 Why Coffee is Perfect for This

- **Real-world sensory data** from professional tasters
- **Rich attribute space** (11 interacting dimensions)
- **Interpretable concepts** (matches human coffee vocabulary)
- **Clear grounding** (each word maps to sensory profile)
- **Interesting results** (concepts like "balanced", "acidic", "full-bodied")

---

## 🤝 Comparison with Wine Experiment

You now have TWO complete experiments:

| Aspect | Wine (`code/`) | Coffee (`code-coffee/`) |
|--------|----------------|-------------------------|
| Dataset | Wine Quality (6,497 samples) | Coffee Quality (1,084 samples) |
| Attributes | 11 physicochemical | 11 sensory + altitude |
| Scenes | 20K train, 2K test | 20K train, 2K test |
| Status | ✓ Complete | ✓ Complete |
| Domain | Physicochemical properties | Professional sensory scores |

You can run either one - or both for comparison!

---

## 📊 Quick Stats

- **Time to set up**: Done! (I did it for you)
- **Time to run**: 1-3 hours for 100K interactions
- **Time to write report**: 2-3 hours using template
- **Deadline**: Dec 14, 23:59
- **Report length**: Max 2 pages
- **Difficulty**: Easy (everything is ready!)

---

## 🎉 You're All Set!

The hard work is done. Just:
1. Open FCG-Editor
2. Run the commands
3. Fill in the template with your results
4. Submit!

**Good luck with your coffee concept learning experiment!** ☕🤖

---

*Created: December 8, 2024*
*Experiment Type: Grounded Naming Game with Continuous Sensory Data*
*Dataset: Coffee Quality Database from CQI*
