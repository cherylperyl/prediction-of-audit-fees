# requirements: pandas and sklearn
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

data = pd.read_csv("data/Team Project cleaned.csv")
data_audit = data[(data.AUDIT_FEES.isna()==False)]

# replace NA values in busdesc with empty string
data.loc[pd.isna(data.busdesc), "busdesc"] = ""
data_audit.loc[pd.isna(data_audit.busdesc), "busdesc"] = ""

# select rows required and drop the rest
data_audit_text = data_audit[["busdesc","FFI12_desc", "AUDIT_FEES"]]

# add FFI12_desc into the busdesc
data_audit_text["words"] = data_audit_text["busdesc"] + " " + data_audit_text["FFI12_desc"]
data_audit_text = data_audit_text[["AUDIT_FEES", "words", "FFI12_desc"]]

# remove punctuations and numbers from the words
words_cleaned = []
for row in data_audit_text["words"]:
    sentence_cleaned = []
    for word in row.split():
        word = word.strip("'/,.!@#$%^&*()-_1234567890=+[]\{\}\|\";:><?`~").lower()
        sentence_cleaned.append(word)
    words_cleaned.append(" ".join(sentence_cleaned))
data_audit_text["words"] = words_cleaned

# group by busdesc to remove duplicate records, take the mean AUDIT_FEES for duplicate records of busdesc
data_audit_text = data_audit_text.groupby("words").mean().reset_index()

# run count vectorizer to obtain word counts
cv = CountVectorizer(stop_words={'english'}, max_df=0.01)
cv_matrix = cv.fit_transform(data_audit_text["words"])
X = pd.DataFrame(cv_matrix.toarray(), columns=cv.get_feature_names())

# convert to csv to continue code in R
X["AUDIT_FEES"] = data_audit_text["AUDIT_FEES"]
# def extractFFI12_desc(row):
#     row = row.split()
#     return row[-1]
# X["FFI12_desc"] = data_audit_text["words"].apply(extractFFI12_desc)
X.to_csv("data/word counts.csv", index = False)