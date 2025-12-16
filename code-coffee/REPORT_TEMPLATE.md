# The Emergence and Evolution of Concepts
## Practical Session 3 - Coffee Quality Experiment

**Student Name**: [YOUR NAME]
**Date**: December 2024

---

## 1. Dataset Selection and Motivation

### 1.1 Dataset Choice

I selected the **Coffee Quality Database** from the Coffee Quality Institute (CQI) for this grounded naming game experiment.

- **Source**: https://github.com/jldbc/coffee-quality-database
- **Size**: 1,084 coffee samples (after preprocessing)
- **Original size**: 1,311 samples
- **Categories**: Professional coffee cupping scores
- **Instances per category**: Arabica coffee beans from various origins worldwide

### Attributes (11 continuous features):

The dataset contains professional sensory evaluation scores from certified Q-graders:

1. **Aroma** - Fragrance/smell intensity (0-10 scale)
2. **Flavor** - Taste intensity (0-10 scale)
3. **Aftertaste** - Lingering taste quality (0-10 scale)
4. **Acidity** - Brightness/liveliness (0-10 scale)
5. **Body** - Mouthfeel/texture weight (0-10 scale)
6. **Balance** - Harmony of attributes (0-10 scale)
7. **Uniformity** - Consistency across cups (0-10 scale)
8. **Clean-Cup** - Absence of defects (0-10 scale)
9. **Sweetness** - Natural sweetness (0-10 scale)
10. **Moisture** - Bean moisture content (percentage)
11. **altitude-mean-meters** - Growing altitude (meters, normalized)

### 1.2 Motivation

I chose the Coffee Quality Dataset for several reasons:

1. **Real-world sensory data**: Professional coffee cupping represents real human sensory perception, making the task ecologically valid and interpretable.

2. **Rich continuous attribute space**: Unlike simple categorical data, coffee quality involves multiple interacting continuous dimensions (flavor, aroma, body, etc.), creating a complex conceptual space for agents to navigate.

3. **Expert annotations**: Data comes from certified Q-graders following standardized protocols, ensuring quality and consistency.

4. **Interpretable results**: The emergent concepts (e.g., "balanced", "acidic", "full-bodied") have clear real-world meanings that can be validated against human coffee vocabulary.

5. **Universal appeal**: Coffee is a globally consumed product, making the domain accessible and interesting to discuss.

### 1.3 Data Splits

Following the assignment requirements:

- **Training scenes**: 20,000 scenes (90% split)
- **Test scenes**: 2,000 scenes (10% split)
- **Scene composition**: 3-10 random coffee samples per scene
- **Total unique coffees**: 1,084 samples

---

## 2. Preprocessing Choices

### 2.1 Attribute Selection

From the original 44 columns in the raw dataset, I selected the 11 most relevant attributes:
- 9 sensory cupping scores (aroma through sweetness)
- 1 physical property (moisture)
- 1 environmental factor (altitude)

**Rationale**: These attributes represent the core sensory profile of coffee that professional tasters use to describe and distinguish coffees. Other columns (origin names, processing dates, etc.) were excluded as they are categorical or non-sensory.

### 2.2 Data Cleaning

**Missing values**: Removed 227 samples with missing values (1,311 → 1,084 samples)
- **Rationale**: Ensures all coffee samples have complete sensory profiles

**Column naming**: Replaced dots and spaces with dashes (e.g., "Clean.Cup" → "Clean-Cup")
- **Rationale**: Required by the FCG-Editor system

### 2.3 Normalization

Applied min-max normalization to scale all attributes to [0, 1] range:
```
normalized_value = (value - min) / (max - min)
```

**Rationale**:
- Prevents high-magnitude attributes (e.g., altitude in meters) from dominating
- Enables fair comparison across different measurement scales
- Required for the distance-based similarity computations in the experiment

---

## 3. Experimental Dynamics

### 3.1 Evolutionary Dynamics

#### Training Configuration:
- **Population size**: 10 agents
- **Training interactions**: 100,000
- **Scene size**: 3-10 coffee samples per scene
- **Learning**: Lateral inhibition with alignment

#### Results Summary:

| Metric | Initial | Final (100K) | Peak |
|--------|---------|--------------|------|
| **Communicative Success** | [INSERT]% | [INSERT]% | [INSERT]% |
| **Lexicon Coherence** | [INSERT]% | [INSERT]% | [INSERT]% |
| **Unique Forms** | [INSERT] | [INSERT] | [INSERT] |

#### 3.1.1 Communicative Success

[DESCRIBE THE TREND - Example:]
Communicative success rose steadily from approximately [X]% at the beginning to [Y]% after 100,000 interactions. The curve shows a typical learning trajectory with rapid initial growth (0-20K interactions), followed by gradual improvement and stabilization around [Z]K interactions.

#### 3.1.2 Construction Inventory Size (Lexicon Size)

[DESCRIBE THE VOCABULARY - Example:]
Agents converged on approximately [N] distinct word forms. This number is [higher/lower/similar to] the number of strongly discriminable coffee types in the dataset, suggesting that agents learned to make [coarse/fine]-grained distinctions.

#### 3.1.3 Conventionality (Lexical Coherence)

[DESCRIBE AGREEMENT - Example:]
Lexical coherence increased from [X]% to [Y]%, indicating that agents increasingly agreed on which words to use for which coffee profiles. The final coherence of [Y]% shows [strong/moderate/weak] alignment across the population.

### 3.2 Emergent Linguistic Conventions

#### Number of Concepts: [INSERT COUNT]

Using `display-lexicon` with weight-threshold 0.1, I identified [N] major concepts that emerged:

[LIST CONCEPTS - Example:]

**Concept 1: "[WORD-FORM]"**
- **Attributes captured**: High [attribute1], high [attribute2], moderate [attribute3]
- **Interpretation**: Likely represents "[balanced/acidic/full-bodied/etc.]" coffees
- **Weight/entrenchment**: [X.XX]

**Concept 2: "[WORD-FORM]"**
- **Attributes captured**: High [attribute], low [attribute]
- **Interpretation**: [Describe]
- **Weight/entrenchment**: [X.XX]

[Continue for each major concept...]

### 3.3 Attribute Importance

Based on the learned concepts, the most distinctive attributes appear to be:
1. [Attribute] - Featured in [N] concepts
2. [Attribute] - Featured in [N] concepts
3. [Attribute] - Featured in [N] concepts

**Observation**: [Discuss why certain attributes were more useful - e.g., "Acidity and body showed the highest variance and were most useful for discrimination, while uniformity showed less variance and was less informative."]

---

## 4. Evaluation on Test Set

### 4.1 Test Configuration

- **Test interactions**: 10,000
- **Alignment**: Disabled (no learning)
- **Dataset**: Unseen test scenes

### 4.2 Test Set Performance

| Metric | Training (final) | Test Set | Difference |
|--------|------------------|----------|------------|
| **Communicative Success** | [X]% | [Y]% | [±Z]% |
| **Lexicon Coherence** | [X]% | [Y]% | [±Z]% |

### 4.3 Generalization Analysis

[DISCUSS RESULTS - Example:]
The test set performance of [Y]% is [slightly lower/comparable to/higher than] the training performance, indicating [good/moderate/poor] generalization. This [was/was not] expected because [explain reasoning - e.g., "the test coffees come from the same distribution as training" or "some test coffees may have extreme attribute combinations not seen during training"].

### 4.4 Accuracy vs. Communicative Success Trade-off

[DISCUSS - Example:]
The communicative success depends on both perceptual accuracy (correctly categorizing the coffee) and linguistic conventionality (using the agreed-upon word). The [X]% test success suggests that agents successfully learned concepts that transfer to novel instances, demonstrating genuine concept learning rather than simple memorization.

---

## 5. Discussion

### 5.1 Interpretation of Results

**Were results as expected?**
[Your interpretation - Example: "Yes, the emergence of [N] concepts aligns with the [N] major sensory profiles in professional coffee cupping (balanced, acidic, sweet, etc.)"]

**Key findings:**
1. [Finding 1]
2. [Finding 2]
3. [Finding 3]

### 5.2 Concept Groundedness

The emergent concepts show clear grounding in the continuous sensory attributes:
- **"[Concept-1]"** → [Describe physical/sensory meaning]
- **"[Concept-2]"** → [Describe physical/sensory meaning]

This demonstrates that agents learned meaningful categories based on the underlying sensory data, not arbitrary labels.

### 5.3 Limitations and Future Work

**Limitations:**
1. [Limitation 1 - e.g., "Altitude might be underutilized due to lower variance"]
2. [Limitation 2 - e.g., "Scene sizes of 3-10 might not capture full distribution"]
3. [Limitation 3]

**Future directions:**
- Investigate which attribute combinations are most distinctive
- Test with different scene sizes or population sizes
- Compare emergent vocabulary to human coffee terminology

### 5.4 Theoretical Implications

This experiment demonstrates:
- **Self-organization**: Agents converge on shared conventions without central coordination
- **Grounded semantics**: Words acquire meaning through perceptual experience
- **Categorical perception**: Agents carve continuous sensory space into discrete concepts
- **Generalization**: Learned concepts transfer to unseen instances

---

## 6. Conclusion

This experiment successfully applied the concept learning methodology to the Coffee Quality Dataset. A population of 10 agents established a shared vocabulary of approximately [N] concepts for communicating about coffee sensory profiles, achieving [X]% communicative success on training data and [Y]% on unseen test data.

The emergent concepts capture meaningful distinctions in the sensory space (e.g., acidic vs. balanced vs. full-bodied coffees), demonstrating that grounded naming games can learn interpretable category systems from continuous multidimensional data.

The results validate the hypothesis that agents can self-organize communication systems grounded in complex real-world sensory domains, with implications for understanding both artificial language emergence and natural concept formation.

---

## References

[1] Coffee Quality Database. GitHub repository. https://github.com/jldbc/coffee-quality-database

[2] Van Eecke, P., & Beuls, K. (2024). Emergent Communication in Continuous Worlds: Self-Organisation of Conceptually Grounded Vocabularies at Scale. [Conference/Journal details from assignment]

[3] Specialty Coffee Association. Coffee Cupping Protocols. https://sca.coffee/

---

**Appendix**: Preprocessing code available in `preprocess_coffee.py`
