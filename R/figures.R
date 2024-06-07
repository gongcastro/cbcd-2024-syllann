library(tibble)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ggdist)
library(glue)
library(purrr)

theme_set(theme_ggdist() +
            theme(panel.grid.major.y = element_line(colour = "grey",
                                                    linetype = "dotted")))

# plotting stimuli
env <- read.csv(file.path("data", "complete-dataset.csv")) |>
  as_tibble() |> 
  select(-X) |> 
  pivot_longer(-c(file, label), 
               names_prefix = "X",
               names_to = "frame", 
               names_transform = as.numeric)


ggplot(env, aes(frame, value, colour = label)) +
  facet_wrap(~label) +
  geom_line(aes(group = interaction(label, file)),
            alpha = 1/30, colour = "grey") +
  geom_smooth() +
  theme(axis.title.x = element_blank()) +
  
  ggplot(env, aes(frame, value, colour = label)) +
  geom_smooth() +
  
  plot_layout(ncol = 1, guides = "collect") &
  plot_annotation(tag_levels = "A") &
  labs(x = "Time frame", y = "Amplitude (normalised dB)",
       colour = "Syllabic structure") &
  scale_colour_brewer(palette = "Dark2") &
  theme(legend.position = "top")

ggsave(file.path("img", "envelopes.png"), width = 6, height = 5)

# plot train-test split
train <- read.csv(file.path("data", "train-dataset.csv")) |>
  as_tibble() |> 
  select(-X)

test <- read.csv(file.path("data", "test-dataset.csv")) |>
  as_tibble() |> 
  select(-X)

datasets <- bind_rows(train, test, .id = "dataset") |> 
  mutate(dataset = if_else(dataset==1, "Train", "Test"),
         dataset = factor(dataset, levels = c("Train", "Test")),
         diphone = unlist(map(strsplit(file, "_"), 2)),
         speaker = gsub("\\.WAV$", "", unlist(map(strsplit(file, "_"), 3))),
         label = if_else(label==1, "CV", "VC"))

diphones <- datasets |> 
  distinct(dataset, diphone, label) |> 
  count(dataset, label)

datasets |> 
  count(dataset, label) |> 
  ggplot(aes(x = dataset, y = n,
             fill = label)) +
  geom_col(position = position_dodge(width = 0.95)) +
  geom_text(aes(label = paste0(format(n, big.mark = ","),
                               "\naudios"),
                y = 3000),
            vjust = 1,
            colour = "black",
            size = 3.5,
            position = position_dodge(width = 0.95)) +
  geom_text(data = diphones,
            aes(label = paste0(n, "\ndiphones"),
                y = 2500,
                colour = "black"),
            vjust = 1,
            size = 3.5,
            colour = "black",
            position = position_dodge(width = 0.95)) +
  labs(x = "Dataset", y = "Number of items",
       colour = "Syllabic structure",
       fill = "Syllabic structure") +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "top") 

ggsave(file.path("img", "training-dataset.png"), height = 5, width = 7)

# plot learning history
history <- read.csv(file.path("data", "history.csv")) |>
  as_tibble() |> 
  rename(epoch = X) |> 
  pivot_longer(-epoch, names_to = "metric") |> 
  mutate(dataset = if_else(grepl("val_", metric), "Validation", "Training"),
         metric = case_when(grepl("acc", metric) ~ "Accuracy (%)",
                            grepl("loss", metric) ~ "Loss",
                            grepl("auc", metric) ~ "AUC (%)"))

ggplot(history, aes(epoch, value, colour = dataset)) +
  facet_wrap(~metric, scales = "free") +
  geom_line(linewidth = 3/4) +
  labs(x = "Epoch", colour = "Dataset") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  theme(axis.title.y = element_blank(),
        legend.position = "top")

ggsave(file.path("img", "metrics.png"), width = 8, height = 5)

# plot test predictions
predictions <- read.csv(file.path("data", "predictions.csv")) |>
  as_tibble() |> 
  select(-X) |> 
  rename(file = X0, prob = X1) |> 
  mutate(label = unlist(map(strsplit(file, "_"), 1)),
         diphone = unlist(map(strsplit(file, "_"), 2)),
         speaker = gsub("\\.WAV$", "", unlist(map(strsplit(file, "_"), 3))),
         prob = as.numeric(gsub("\\[|\\]", "", prob)),
         pred = if_else(prob >= 0.5, "CV", "VC"),
         is_correct = pred==label)

predictions |> 
  summarise(accuracy = mean(is_correct),
            .by = c(label)) |> 
  ggplot(aes(label, accuracy, fill = label)) + 
  geom_col(colour = "white") +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.001)),
            position = position_nudge(y = -0.1)) +
  labs(x = "Syllabic structure",
       y = "Accuracy") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "none")

ggsave(file.path("img", "test-accuracy.png"),
       width = 6, height = 4)

# plot test evaluation
roc <- read.csv(file.path("data", "roc.csv")) |> 
  as_tibble() |> 
  rename(epoch = X) 

auc <- unique(roc$auc)

ggplot(roc, aes(fpr, tpr)) +
  geom_area(fill = "grey95") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              colour = "grey") +
  geom_line(linewidth = 3/4) +
  labs(x = "False Positive Rate (%)", 
       y = "True positive rate (%)",
       title = glue("ROC (AUC = {round(auc, 3)})")) +
  coord_equal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "top")

ggsave(file.path("img", "roc.png"), width = 5, height = 5)


