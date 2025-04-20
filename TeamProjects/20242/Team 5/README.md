# 🚀 Beyond the Questions : Strategic Interview Data Analysis

This project was conducted as part of the **Text Mining** course.  

---

## 👥 Team 5 Members Information

| **Student ID** | **Name**        | **GitHub URL**                                 |
|----------------|-----------------|------------------------------------------------|
| 21900707       | YoungWoo Cho    | [jayjo9](https://github.com/jayjo9)            |
| 22200022       | Jiho Kang       | [hahahohoJIHO](https://github.com/hahahohoJIHO)|
| 22200130       | Yewon Kim       | [daydayyewon](https://github.com/daydayyewon)  |

---

## 📌 Project Introduction

This project leverages the **[AI-Hub Recruitment Interview Dataset](https://www.aihub.or.kr/aihubdata/data/view.do?currMenu=115&topMenu=100&aihubDataSe=data&dataSetSn=71592)** to uncover hidden topics, evaluation elements, and essential keywords in interview questions. It aims to provide strategies for successful interviews.

---

## 💡 Project Background

Everyone faces interviews at some point in their lives.  
However, interview preparation can be daunting, and strategies must differ based on gender, job type, and experience level.  
This project seeks to offer tailored preparation strategies for successful interviews.

---

## 🎯 Project Objectives
1. **Analyze Interview Question Patterns:** 
   - Identify question trends by job category.
   - Examine differences in questions based on gender and experience level.
2. **Explore Key Keywords and Topics:**
   - Conduct topic modeling using Latent Dirichlet Allocation (LDA) and Structural Topic Model (STM).
3. **Propose Strategic Preparation Directions:**
   - Provide job-specific question examples.
   - Extract insights for interview preparation.

---

## 🛠 Technologies Used
- **Programming Language:** R
- **Libraries:**
  - 📄 Data Processing: `dplyr`, `tidyr`, `jsonlite`
  - 📊 Visualization: `ggplot2`, `patchwork`, `ggraph`
  - 🔍 Text Analysis: `tm`, `tidytext`, `KoNLP`, `topicmodels`
  - 📈 Machine Learning: `LDAvis`, `stm`

---

## 🛠️ Installation and Execution

### 1️⃣ Install Necessary Software
- R (version ≥ 4.0.0)
- RStudio (optional)

### 2️⃣ Install Required Libraries
Run the following code in R:

```R
install.packages(c("dplyr", "tidyr", "jsonlite", "ggplot2", "patchwork", 
                   "ggraph", "KoNLP", "topicmodels", "LDAvis", "stm"))
```

---

## 📂 Project Structure

### 1️⃣ Data Preparation

#### 1.1 Dataset Download
- **Source:** Downloaded from AI-Hub's official website.
- **Dataset Details:**
  - 📦 **Data Information**
    - **Year Built:** 2022  
    - **Last Updated:** December 2023  
    - **Data Type:** Audio (wav), Text (json)  
    - **Data Size:** 162.08 GB  
    - **Number of Documents:** Approximately 67,307  
  - **Fields Extracted:**
    - `Question`, `Category`, `Gender`, `Experience`
  - **Preprocessing:**
    - Removed 202 entries due to lexical errors. 

| **Field**      | **Description**                       |
|----------------|---------------------------------------|
| `Question`     | Text of interview questions           |
| `Category`     | Job type (e.g., Management, ICT)      |
| `Gender`       | Gender (Male, Female)                |
| `Experience`   | Experience level (New, Experienced)  |
| `words`        | Extracted nouns from question text    |


#### 1.2 Data Exploration
- **Keyword Frequency Analysis:** Identified most frequently occurring words.
- **Visualization:**
  - Histograms of question length (character/word count).
  - Keyword distributions by job category.

---


### 2️⃣ Data Preprocessing
- **Objective:** Prepare the dataset for effective text analysis.
- **Key Steps:**
  - Checked for outliers (none found).
  - Addressed data imbalance by balancing combinations (~500 per group).
  - Calculated character and word counts for each question.
  - Removed unnecessary symbols and single-character words.
  - Extracted nouns using `KoNLP`.
  - Applied stopwords to eliminate irrelevant terms.

 ![Most Frequently Used Words in Job Interviews after Preprocessing](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Most%20Frequently%20Used%20Words%20in%20Job%20Interviews%20after%20Preprocessing.png)

---

### 3️⃣ 텍스트 분석

#### 3.1 Key Keyword Analysis
- **TF-IDF Calculation:** Identified distinct keywords by job category.
- **Gender/Experience Analysis:** Compared primary keywords by demographic factors.

![Keyword Analysis By Job Category 1](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Keyword%20Analysis%20By%20Job%20Category%201.png)
![Keyword Analysis By Job Category 2](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Keyword%20Analysis%20By%20Job%20Category%202.png)
![Keyword Analysis By Gender](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Keyword%20Analysis%20By%20Gender.png)
![Keyword Analysis By Experienced](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Keyword%20Analysis%20By%20Experienced.png)

#### 3.2 Topic Modeling
- **LDA Model:** Explored hidden topics within the dataset.
- **STM Model:** Analyzed topics incorporating metadata (e.g., job, gender, experience).

#### 3.3 Network Analysis
- **Word Network:** Visualized relationships between major keywords.
- Generated potential new interview questions based on findings.

![Network Analysis 'Public Service'](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Network%20Analysis%20Public%20Service.png)
![Network Analysis 'RND'](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/Network%20Analysis%20RND.png)

---

## 🔍 LDA Analysis Results

### 1️⃣ Perplexity Analysis
- **Key Findings:**
  - Lowest Perplexity observed at **k=2**, suggesting that the dataset can be effectively modeled with a small number of topics.
  - Exploring higher values such as **k=3** or **k=4** could provide additional interpretability.

![LDA Perplexity](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/LDA%20Perplexity.png)

---

### 2️⃣ **k = 2 ~ 4 결과 요약**
### 2️⃣ Results for k = 2 ~ 4

| **k Value** | **Topic** | **Keywords**                        | **Summary**                           |
|-------------|-----------|--------------------------------------|---------------------------------------|
| **k=2**     | Topic 1   | solution, explain, question          | Problem-solving-focused questions     |
|             | Topic 2   | say, nowadays, curious              | Observational/conversational questions|
| **k=3**     | Topic 1   | solution, experience, candidate     | Problem-solving and career-related    |
|             | Topic 2   | say, topic, curious                 | Observational/advisory questions      |
|             | Topic 3   | challenge, wish, passion            | Motivation and enthusiasm             |
| **k=4**     | Topic 1   | solution, support, problem          | Practical problem-solving             |
|             | Topic 2   | say, nowadays, curious              | Neutral/conversational questions      |
|             | Topic 3   | experience, candidate, resume       | Career and background                 |
|             | Topic 4   | passion, challenge, wish            | Attitude and aspirations evaluation   |


---

### 3️⃣ Insights

#### Common Patterns
- Across all **k values**, keywords like 'solution', 'experience', and 'candidate' are central, showing a focus on **problem-solving skills** and **job fit**.

#### Differences by k Value
- **k=2**: Broadly classifies questions.
- **k=3**: Segments into problem-solving, observational questions, and motivation evaluation.
- **k=4**: Distinguishes between job skills, observational questions, career-related questions, and attitude/motivation evaluation.

---

### 4️⃣ Topic-Specific Insights for k=3

| **Topic**  | **Keywords**                       | **Summary**                                             |
|------------|------------------------------------|-------------------------------------------------------|
| Topic 1    | job, role, work, entry            | Evaluates understanding of the job and role readiness. |
| Topic 2    | time, management, people, coping  | Focuses on time management, interpersonal skills, and problem-solving. |
| Topic 3    | stress, goal, collaboration       | Emphasizes stress management, goal achievement, and teamwork. |

![LDA Example](https://github.com/hahahohoJIHO/24-2TextMiningProject/blob/main/images/LDA%20Example.png)

---
## 🔍 STM Analysis Results

### 1️⃣ Top Keywords by Topic

| **Topic**  | **Highest Prob**                  | **FREX**                      | **Lift**                   | **Score**                  |
|------------|----------------------------------|--------------------------------|----------------------------|----------------------------|
| Topic 1    | 어떻게, 회사에, 건가요           | 있습니다, 회사에, 건가요        | 가보고, 가정합시다          | 어떻게, 회사에, 건가요      |
| Topic 2    | 그리고, 무엇을, 있나요           | 그리고, 무엇을, 있으신지        | 거쳤는지, 과정들을          | 그것을, 그리고, 존경하는    |
| Topic 3    | 바랍니다, 말씀해, 생각하는       | 바랍니다, 이유도, 생각하는       | 사용하시는지, 진출했으면     | 바랍니다, 생각하는, 분야에  |
| Topic 4    | 있다면, 협업을, 스트레스를        | 주시면, 협업을, 기억에          | 가능하시겠어요, 가정과       | 스트레스를, 기억에, 방법으로|
| Topic 5    | 주세요, 경험이, 본인의           | 본인의, 자세하게, 주시고         | 가능하면, 가지거나          | 주세요, 본인의, 극복하기    |

---

### 2️⃣ Topic-Specific Insights

| **Topic**  | **Keywords**                   | **Summary**                                                                 |
|------------|-------------------------------|-----------------------------------------------------------------------------|
| Topic 1    | 어떻게, 회사에, 건가요          | Evaluates interest in the company or role and motivation to apply.          |
| Topic 2    | 그리고, 무엇을, 있나요          | Explores personal values, challenging experiences, or role models.          |
| Topic 3    | 바랍니다, 말씀해, 생각하는      | Centers on personal values, goals, and motivations.                         |
| Topic 4    | 있다면, 협업을, 스트레스를       | Focuses on collaboration, stress management, and conflict resolution skills. |
| Topic 5    | 주세요, 경험이, 본인의          | Evaluates personal experiences, overcoming challenges, strengths, and weaknesses. |

---

## 🚀 Utilization
### **For Candidates**
- 🌟 Prepare tailored answers focusing on **problem-solving**, **interpersonal skills**, and **stress management**.
- 🌟 Practice responses based on analyzed question types and key topics.

### **For Companies**
- ✅ Develop distinct interview questions per job category.
- ✅ Leverage data insights to improve candidate evaluation systems:
  - Utilize topic-specific findings to diversify questions and assess candidates' strengths quantitatively.

---

## 📚 References

- **AI-Hub Recruitment Interview Dataset**  
  [AI-Hub Dataset Link](https://www.aihub.or.kr/aihubdata/data/view.do?currMenu=115&topMenu=100&aihubDataSe=data&dataSetSn=71592)

---

## 🌟 License

This project is distributed under the MIT License.  
For more details, refer to the [LICENSE file](./LICENSE).
