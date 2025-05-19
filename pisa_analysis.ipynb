import gspread
import pandas as pd
from google.auth import default
from google.colab import auth
auth.authenticate_user()
creds, _ = default()
gc = gspread.authorize(creds)
spreadsheet = gc.open_by_key('1M0QOhfSeSyZXyTe7CG0HK2Qn63eyhgE_oPDthn-AevE')
worksheet = spreadsheet.get_worksheet(0)
df = pd.DataFrame(worksheet.get())
# Code for making the first row as header. Remove if not needed.
df.columns = df.iloc[0]
df = df.drop(0)
df.head()

# Required libraries
try:
    import pandas as pd
    import numpy as np
    import matplotlib.pyplot as plt
    import seaborn as sns
except ModuleNotFoundError as e:
    print("A required module is not installed:", e)
    print("Please make sure pandas, numpy, matplotlib, and seaborn are installed.")
    raise

# Set seaborn theme for consistent plotting
sns.set(style="whitegrid")

# Example data: OECD average scores (2018 and 2022)
OECD2018AverageScore = pd.DataFrame({
    "Subject": ["Reading", "Mathematics", "Science"],
    "Score": [487, 489, 489]
})

OECD2022AverageScore = pd.DataFrame({
    "Subject": ["Reading", "Mathematics", "Science"],
    "Score": [476, 472, 485]
})

# Example DepEd budget data
DepEd_Budget_data = pd.DataFrame({
    "Year": [2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025],
    "Budget": [580.6, 528.8, 500.0, 606.6, 631.8, 710.6, 717.0, 737.1]
})

# Load PISA scores directly from Google Sheets
sheet_url = "https://docs.google.com/spreadsheets/d/1M0QOhfSeSyZXyTe7CG0HK2Qn63eyhgE_oPDthn-AevE/export?format=xlsx"

# Read the two sheets into DataFrames
PISA_2018_scores = pd.read_excel(sheet_url, sheet_name="PISA 2018")
PISA_2022_scores = pd.read_excel(sheet_url, sheet_name="PISA 2022")

# Descriptive statistics function
def descriptive_stats_function():
    desc2018 = PISA_2018_scores.copy()
    desc2022 = PISA_2022_scores.copy()
    desc2018["Year"] = 2018
    desc2022["Year"] = 2022

    combined = pd.concat([desc2018, desc2022], ignore_index=True)

    # Reshape the DataFrame from wide to long format
    PISA_long = pd.melt(combined,
                        id_vars=["Country", "Year"],
                        value_vars=["Reading", "Mathematics", "Science"],
                        var_name="Subject", value_name="Score")
    PISA_long.dropna(subset=["Score"], inplace=True)

    # Group by Year and Subject and calculate statistics
    descriptive_stats = PISA_long.groupby(["Year", "Subject"]).agg(
        Min=("Score", "min"),
        Q1=("Score", lambda x: x.quantile(0.25)),
        Median=("Score", "median"),
        Q3=("Score", lambda x: x.quantile(0.75)),
        Max=("Score", "max"),
        Mean=("Score", "mean"),
        SD=("Score", "std"),
        Variance=("Score", "var"),
        Range=("Score", lambda x: x.max() - x.min()),
        IQR=("Score", lambda x: x.quantile(0.75) - x.quantile(0.25))
    ).reset_index()

    return descriptive_stats

# Call the function (you can print or inspect this later)
descriptive_statistics = descriptive_stats_function()
print(descriptive_statistics)

# Violin Plot Function Template (shared logic)
def draw_violin_plot(data, year):
    if data.empty:
        print(f"PISA {year} data is empty. Load the data first.")
        return

    # Reshape the data
    long_df = pd.melt(data,
                      id_vars="Country",
                      value_vars=["Reading", "Mathematics", "Science"],
                      var_name="Subject", value_name="Score")

    # Create sample size labels
    sample_sizes = long_df.groupby("Subject").size().reset_index(name="n")
    long_df = long_df.merge(sample_sizes, on="Subject")
    long_df["SubjectLabel"] = long_df["Subject"] + "\n" + "n=" + long_df["n"].astype(str)

    # Order by median
    subject_order = long_df.groupby("SubjectLabel")["Score"].median().sort_values(ascending=False).index
    long_df["SubjectLabel"] = pd.Categorical(long_df["SubjectLabel"], categories=subject_order, ordered=True)

    # Min/Max countries
    min_scores = long_df.loc[long_df.groupby("SubjectLabel")["Score"].idxmin()]
    max_scores = long_df.loc[long_df.groupby("SubjectLabel")["Score"].idxmax()]

    # Quartiles
    quartiles = long_df.groupby("SubjectLabel")["Score"].quantile([0.25, 0.5, 0.75]).unstack()
    quartiles.columns = ["Q1", "Median", "Q3"]
    quartiles = quartiles.reset_index()

    # Plot
    plt.figure(figsize=(10, 6))
    sns.violinplot(data=long_df, x="Score", y="SubjectLabel", inner=None, palette="pastel")
    sns.boxplot(data=long_df, x="Score", y="SubjectLabel", whis=[0, 100], width=0.1, showcaps=False, fliersize=0, boxprops={'facecolor':'None'})

    # Min/Max points
    plt.scatter(min_scores["Score"], min_scores["SubjectLabel"], color="red", label="Min", zorder=10)
    plt.scatter(max_scores["Score"], max_scores["SubjectLabel"], color="blue", label="Max", zorder=10)

    plt.title(f"PISA {year} Score Distribution with Min/Max and IQR Labels")
    plt.xlabel("PISA Score")
    plt.ylabel("")
    plt.legend()
    plt.tight_layout()
    plt.show()

# Violin plots for 2018 and 2022
draw_violin_plot(PISA_2018_scores, 2018)
draw_violin_plot(PISA_2022_scores, 2022)

# Correlation: DepEd Budget vs PH PISA Scores
def plot_budget_vs_scores():
    ph_2018 = PISA_2018_scores[PISA_2018_scores["Country"] == "Philippines"].copy()
    ph_2022 = PISA_2022_scores[PISA_2022_scores["Country"] == "Philippines"].copy()
    ph_2018["Year"] = 2018
    ph_2022["Year"] = 2022
    ph_scores = pd.concat([ph_2018, ph_2022], ignore_index=True)
    ph_scores = ph_scores.merge(DepEd_Budget_data, on="Year", how="left")

    ph_long = pd.melt(ph_scores, id_vars=["Country", "Year", "Budget"],
                      value_vars=["Reading", "Mathematics", "Science"],
                      var_name="Subject", value_name="Score")

    plt.figure(figsize=(8, 5))
    sns.scatterplot(data=ph_long, x="Budget", y="Score", hue="Subject", s=100)
    sns.regplot(data=ph_long[ph_long.Subject == "Reading"], x="Budget", y="Score", scatter=False, color="steelblue", label=None, line_kws={"linestyle":"dashed"})
    sns.regplot(data=ph_long[ph_long.Subject == "Mathematics"], x="Budget", y="Score", scatter=False, color="darkred", label=None, line_kws={"linestyle":"dashed"})
    sns.regplot(data=ph_long[ph_long.Subject == "Science"], x="Budget", y="Score", scatter=False, color="darkgreen", label=None, line_kws={"linestyle":"dashed"})

    for _, row in ph_long.iterrows():
        plt.text(row["Budget"], row["Score"] + 2, str(row["Year"]), fontsize=9, ha="center")

    plt.title("Relationship Between DepEd Budget and PISA Scores (Philippines)")
    plt.xlabel("DepEd Budget (Billion PHP)")
    plt.ylabel("PISA Score")
    plt.tight_layout()
    plt.show()

# Show the plot
plot_budget_vs_scores()


# Line Plot: Philippines vs OECD Average over Time
def plot_line_ph_vs_oecd():
    # Add 'Year' and 'Country' to OECD data
    oecd_2018 = OECD2018AverageScore.copy()
    oecd_2022 = OECD2022AverageScore.copy()
    oecd_2018["Year"] = 2018
    oecd_2022["Year"] = 2022
    oecd_2018["Country"] = "OECD Average"
    oecd_2022["Country"] = "OECD Average"

    # Philippines data reshaped
    ph_2018 = PISA_2018_scores[PISA_2018_scores["Country"] == "Philippines"].copy()
    ph_2022 = PISA_2022_scores[PISA_2022_scores["Country"] == "Philippines"].copy()
    ph_2018["Year"] = 2018
    ph_2022["Year"] = 2022
    ph_long = pd.melt(pd.concat([ph_2018, ph_2022]),
                      id_vars=["Country", "Year"],
                      value_vars=["Reading", "Mathematics", "Science"],
                      var_name="Subject", value_name="Score")

    oecd_long = pd.concat([oecd_2018, oecd_2022], ignore_index=True)

    combined = pd.concat([ph_long, oecd_long], ignore_index=True)

    # Plot
    g = sns.FacetGrid(combined, col="Subject", sharey=False, height=4, aspect=1.3)
    g.map_dataframe(sns.lineplot, x="Year", y="Score", hue="Country", marker="o")
    g.set_titles(col_template="{col_name}")
    g.add_legend()
    g.set_axis_labels("Year", "PISA Score")
    plt.subplots_adjust(top=0.85)
    g.fig.suptitle("PISA Scores: Philippines vs OECD Average (2018 & 2022)")
    plt.show()

# Show the line plot
plot_line_ph_vs_oecd()


# Helper to reshape any year's scores for subject-based plots
def reshape_pisa_scores(df, year):
    df_long = pd.melt(df,
                      id_vars="Country",
                      value_vars=["Reading", "Mathematics", "Science"],
                      var_name="Subject", value_name="Score")
    df_long["Year"] = year
    return df_long

# Bar plot comparing Philippines to OECD, top/bottom, and similar-income countries
def bar_ph_vs_world(pisa_scores, year):
    similar_income = ["Indonesia", "Thailand", "Vietnam", "Malaysia", "India", "Brazil"]
    df_long = reshape_pisa_scores(pisa_scores, year)

    # Top & bottom countries per subject
    top_bottom = df_long.groupby("Subject").agg(MaxScore=('Score', 'max'), MinScore=('Score', 'min')).reset_index()
    top_countries = df_long[df_long.groupby("Subject")["Score"].transform('max') == df_long["Score"]][['Subject', 'Country']]
    bottom_countries = df_long[df_long.groupby("Subject")["Score"].transform('min') == df_long["Score"]][['Subject', 'Country']]
    key_countries = pd.concat([top_countries, bottom_countries])['Country'].unique()

    selected = df_long[df_long['Country'].isin(["Philippines", "OECD Average"] + similar_income + list(key_countries))]
    selected["Group"] = selected["Country"].apply(lambda x:
        "Philippines" if x == "Philippines" else
        "OECD Average" if x == "OECD Average" else
        "Top/Bottom Scorer" if x in key_countries else
        "Similar-Income")

    g = sns.catplot(data=selected, x="Score", y="Country", hue="Group", col="Subject",
                    kind="bar", palette="muted", sharex=False, height=4, aspect=1.2)
    g.set_titles("{col_name}")
    g.set_axis_labels("Score", "Country")
    g.fig.subplots_adjust(top=0.85)
    g.fig.suptitle(f"Philippines vs OECD, Top/Bottom and Similar-Income Countries (PISA {year})")
    plt.show()

# Bar plots for 2018 and 2022
bar_ph_vs_world(PISA_2018_scores, 2018)
bar_ph_vs_world(PISA_2022_scores, 2022)

# Scatter plots comparing all countries to OECD average lines
def scatter_scores_vs_oecd(pisa_scores, oecd_avg, year):
    df_long = reshape_pisa_scores(pisa_scores, year)
    oecd_avg = oecd_avg.rename(columns={"Score": "Average"})

    # Assign distinct colors to OECD lines
    subject_colors = {
        "Reading": "blue",
        "Mathematics": "orange",
        "Science": "green"
    }

    # Increase figure size and adjust spacing
    plt.figure(figsize=(12, 15))
    sns.scatterplot(data=df_long, x="Score", y="Country", hue="Subject", alpha=0.8, s=60)

    for _, row in oecd_avg.iterrows():
        plt.axvline(x=row['Average'], linestyle="--", label=f"OECD {row['Subject']}", color='gray')

    plt.title(f"PISA {year} Scores by Country with OECD Average Lines")
    plt.xlabel("Score")
    plt.ylabel("Country")
    plt.legend()
    plt.tight_layout()
    plt.show()

# Scatter plots for 2018 and 2022
scatter_scores_vs_oecd(PISA_2018_scores, OECD2018AverageScore, 2018)
scatter_scores_vs_oecd(PISA_2022_scores, OECD2022AverageScore, 2022)
