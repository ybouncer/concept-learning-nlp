# ☕ Coffee Experiment - Submission Checklist

## 📦 What to Submit

Create a **single ZIP file** containing exactly **2 files**:

### ✅ Required Files:

1. **`report.pdf`** (max 2 pages)
   - Use `REPORT_TEMPLATE.md` as your starting point
   - Fill in your actual experimental results
   - Convert to PDF

2. **`coffee.ipynb`** (Jupyter notebook)
   - Located at: `code-coffee/create-dataset/coffee.ipynb`
   - Shows your data preprocessing steps
   - Already complete and ready to submit!

---

## 📝 Report Writing Steps

### 1. Run the FCG-Editor Experiment

Execute in FCG-Editor:
```lisp
(setup-experiment "coffee")
(setup-train)
(loop for i from 1 to 100000 do (run-interaction *experiment*))

(notify reset-monitors)
(setup-test)
(loop for i from 1 to 10000 do (run-interaction *experiment*))

(display-lexicon (first (agents *experiment*)) :weight-threshold 0.1 :sort t)
```

### 2. Collect Your Results

**From CSV files** (`output/experiment/coffee/logging/`):
- Final communicative success %
- Final lexicon coherence %
- Unique forms count

**From `display-lexicon` output**:
- List of emergent concepts
- Which attributes each concept captures
- Entrenchment scores

### 3. Fill in the Report Template

Open `REPORT_TEMPLATE.md` and replace placeholders:

**Section 3.1** - Insert metrics:
```markdown
| Metric | Initial | Final (100K) |
|--------|---------|--------------|
| Communicative Success | 10% | 89% |  ← Your actual numbers
| Lexicon Coherence | 5% | 84% |      ← Your actual numbers
```

**Section 3.2** - Add concepts:
```markdown
**Concept 1: "wug-42"**
- Attributes: High acidity (0.85), high aroma (0.78)
- Interpretation: Bright, acidic coffees
- Weight: 0.92
```

**Section 4** - Add test results:
```markdown
| Metric | Training | Test | Difference |
|--------|----------|------|------------|
| Communicative Success | 89% | 87% | -2% |  ← Your numbers
```

### 4. Convert to PDF

Options:
- Use Markdown to PDF converter (e.g., Pandoc, Typora)
- Copy to Word/Google Docs and export as PDF
- Use online converters (markdown-to-pdf.com)

**Ensure**:
- Max 2 pages
- All sections included
- Figures/tables readable

---

## 📂 Creating the ZIP File

### Option 1: Using File Explorer (Windows)

1. Create a new folder called `coffee_submission`
2. Copy these files into it:
   - `report.pdf`
   - `coffee.ipynb`
3. Right-click the folder → "Send to" → "Compressed (zipped) folder"
4. Rename to: `YourName_coffee_experiment.zip`

### Option 2: Using Command Line

```bash
cd "c:\Users\yboun\Downloads\grounded_naming_game\grounded_naming_game 2\code-coffee"

# Create submission folder
mkdir submission
cp report.pdf submission/
cp create-dataset/coffee.ipynb submission/

# Create ZIP
# (Use 7-Zip, WinRAR, or built-in Windows compression)
```

---

## ✅ Pre-Submission Checklist

Before submitting, verify:

### Report (report.pdf)
- [ ] Max 2 pages
- [ ] PDF format
- [ ] All sections completed:
  - [ ] Dataset selection and motivation
  - [ ] Preprocessing choices
  - [ ] Experimental results (with YOUR actual numbers)
  - [ ] Emergent concepts described
  - [ ] Test set performance
  - [ ] Discussion
- [ ] Figures/tables readable
- [ ] References included

### Jupyter Notebook (coffee.ipynb)
- [ ] Located at correct path: `create-dataset/coffee.ipynb`
- [ ] All cells present and in order
- [ ] Shows complete preprocessing workflow:
  - [ ] Data download
  - [ ] Attribute selection
  - [ ] Normalization
  - [ ] Metadata creation
  - [ ] Export to CSV

### ZIP File
- [ ] Contains EXACTLY 2 files (no more, no less)
- [ ] Named appropriately (e.g., `YourName_coffee_experiment.zip`)
- [ ] File size reasonable (~50-500 KB for report + notebook)
- [ ] Can be opened and extracted successfully

---

## 🎯 What the Graders Will Check

### Report Quality (50%):
- Clear dataset description and motivation
- Justified preprocessing choices
- Accurate experimental results
- Meaningful interpretation of emergent concepts
- Discussion of test set performance
- Professional presentation

### Technical Correctness (30%):
- Notebook shows proper preprocessing steps
- Data correctly normalized
- Metadata file created properly
- All required attributes included

### Completeness (20%):
- All sections present
- Both deliverables included
- Submitted on time

---

## 📅 Submission Details

**Deadline**: Sunday, December 14 at 23:59

**Where to Submit**: WebCampus (UNamur) or Canvas (VUB)

**Format**: Single ZIP file

**Files**: 2 files (report.pdf + coffee.ipynb)

---

## 💡 Pro Tips

### For the Report:

1. **Be specific**: Don't say "success increased" - say "success increased from 12% to 89%"

2. **Interpret concepts**: Don't just list attributes - explain what they mean
   - ❌ Bad: "Concept-1 has high acidity and aroma"
   - ✅ Good: "Concept-1 (high acidity, high aroma) represents bright, aromatic coffees similar to Ethiopian light roasts"

3. **Use the template**: It's 90% done - just fill in your numbers!

4. **Keep it concise**: 2 pages max - be clear and direct

### For the Notebook:

1. **Don't modify**: The notebook is already complete and correct
2. **Just submit it**: Copy `coffee.ipynb` as-is into your ZIP
3. **Make sure it runs**: All cells should execute without errors

---

## 🚨 Common Mistakes to Avoid

❌ **DON'T**:
- Submit more than 2 files
- Exceed 2 pages for the report
- Include the wrong notebook (e.g., wine.ipynb instead of coffee.ipynb)
- Forget to fill in the actual experimental results
- Submit without running the experiment first
- Include raw data files or scene files in the ZIP

✅ **DO**:
- Run the full experiment (100K training + 10K test)
- Fill in ALL placeholder sections in the template
- Include specific numbers and examples
- Submit as a clean ZIP with only 2 files
- Test that your ZIP can be extracted

---

## 📊 Example Submission Structure

```
YourName_coffee_experiment.zip
├── report.pdf (2 pages, contains YOUR actual results)
└── coffee.ipynb (preprocessing notebook from code-coffee/create-dataset/)
```

**ZIP file should be**: 50 KB - 500 KB

---

## 🆘 Need Help?

If you encounter issues:

1. **Experiment not running**: Check `QUICKSTART.md` or `README.md`
2. **GraphViz errors**: Ignore - experiment still works
3. **Report questions**: Use `REPORT_TEMPLATE.md` - it guides you
4. **Technical issues**: Check the troubleshooting section in `README.md`

---

## 🎉 You're Ready!

Everything is prepared:
- ✅ Dataset processed
- ✅ Scenes generated
- ✅ Notebook complete
- ✅ Report template ready
- ✅ All documentation provided

Just run the experiment, fill in your results, and submit!

**Good luck with your submission!** ☕🤖

---

*Deadline: December 14, 23:59*
*Files: report.pdf + coffee.ipynb*
*Format: Single ZIP file*
