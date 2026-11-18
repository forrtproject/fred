success_criteria_colors <- tibble::tribble(
  ~criterion,                 ~label,                                      ~color,

  # significance_r outcome_report labels
  "significance_r",           "OS not significant",                        "#D3D3D3",
  "significance_r",           "failure",                                   "#FF7F7F",
  "significance_r",           "success",                                   "#8FBC8F",
  "significance_r",           "failure (reversal)",                        "darkred",

  # significance_agg outcome_report labels
  "significance_agg",         "success",                                   "#8FBC8F",
  "significance_agg",         "failure",                                   "#FF7F7F",
  "significance_agg",         "failure (reversal)",                        "darkred",

  # consistency_ci outcome_report labels
  "consistency_ci",           "success",                                   "#8FBC8F",
  "consistency_ci",           "failure",                                   "#FF7F7F",

  # consistency_pi outcome_report labels
  "consistency_pi",           "success",                                   "#8FBC8F",
  "consistency_pi",           "failure",                                   "#FF7F7F",

  # homogeneity outcome_report labels
  "homogeneity",              "success",                                   "#8FBC8F",
  "homogeneity",              "failure",                                   "#FF7F7F",

  # homogeneity_significance outcome_report labels
  "homogeneity_significance", "OS not significant",                        "#D3D3D3",
  "homogeneity_significance", "success (homogeneous and jointly significantly above 0)", "#8FBC8F",
  "homogeneity_significance", "failure (not homogeneous but jointly significantly above 0)", "#FF7F7F",
  "homogeneity_significance", "failure (effects are not homogeneous and not significant)", "#FF7F7F",
  "homogeneity_significance", "failure (homogeneous but not significant)",           "#FF7F7F",
  "homogeneity_significance", "failure (not homogeneous and not significant)",           "#FF7F7F",

  # small_telescopes outcome_report labels
  "small_telescopes",         "success",                                   "#8FBC8F",
  "small_telescopes",         "failure",                                   "#FF7F7F",

  # reported_success outcome_report labels (colours match the fred-apps Explorer)
  "reported_success",         "successful",                                "#8FBC8F",
  "reported_success",         "statistically successful but flawed",       "#d88201",
  "reported_success",         "failed",                                    "#FF7F7F",
  "reported_success",         "informative failure",                       "#FF7F7F",
  "reported_success",         "mixed",                                     "#FFD700",
  "reported_success",         "uninformative",                             "#D3D3D3",
  "reported_success",         "descriptive only",                          "#B0C4DE",
  "reported_success",         "successful replication",                    "#C8C8C8",
  "reported_success",         "failed replication",                        "#C8C8C8",
  "reported_success",         "OS not significant",                        "#D3D3D3",
  "reported_success",         "Not calculable",                            "#C8C8C8"
)

success_criterion_note <- c(
  significance_r = "Replication success was assessed based on the statistical significance of the replication effect (and whether its direction is consistent with the original effect). Replications that were significant and in the same direction as the original were considered as successes, while replications that were not significant or in the opposite direction were considered as failures.",
  significance_agg = "Replication success was assessed based on the aggregation of effect sizes from the original and replication studies using a meta-analytic approach. Replications where the combined effect was significantly different from zero were considered as successes, while those where the combined effect was not significant were considered as failures.",
  consistency_ci = "Replication success was assessed based on whether the original effect size fell within the confidence interval of the replication effect size. Replications where the original effect size was within the confidence interval were considered as successes, while those where it was outside were considered as failures.",
  consistency_pi = "Replication success was assessed based on whether the replication effect size fell within the prediction interval derived from the original study and replication sample size. Replications within the prediction interval were considered as successes, while those outside the prediction interval were considered as failures.",
  homogeneity = "Replication success was assessed based on the homogeneity of the effects from the original and replication studies using a heterogeneity test (Q-test). Replications where the effects were homogeneous were considered as successes, while those that showed heterogeneity were considered as failures.",
  homogeneity_significance = "Replication success was assessed based on the combination of homogeneity and the significance of the effect sizes. Replications where the effects were homogeneous and jointly significantly different from zero were considered as successes, while those that were either not homogeneous or not significantly different from zero were considered as failures.",
  small_telescopes = "Replication success was assessed based on whether the replication effect size was larger than the effect size that would have given the original study a power of 33%. Replications that met this criterion were considered as successes, while those that did not were considered as failures.",
  reported_success = "Replication success follows the outcome reported by the replication authors, as coded in the `reported_success` variable of FReD. Unlike the other criteria, it is not calculated from effect sizes, so it also covers replications without usable effect sizes."
)
