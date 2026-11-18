
RANDOM_SEED <- 42
TEST_PROP   <- 0.20
TARGET      <- "diagnosis"     

PROPERTIES <- c("radius", "texture", "perimeter", "area", "smoothness",
                "compactness", "concavity", "concave_points", "symmetry",
                "fractal_dimension")
STATS <- c("mean", "se", "worst")

FEATURES <- as.vector(outer(PROPERTIES, STATS, paste, sep = "_"))


load_data <- function(path = file.path("data", "wdbc_raw.csv")) {
  if (!file.exists(path)) {
    stop("Can't find ", path, ". Open bc-project.Rproj, or setwd() to the repo folder.")
  }
  raw <- read.csv(path, stringsAsFactors = FALSE)

  stopifnot(all(FEATURES %in% names(raw)),
            all(raw$diagnosis %in% c("M", "B")))

  df <- raw[, c("id", FEATURES)]
  df[[TARGET]] <- ifelse(raw$diagnosis == "M", 1L, 0L)   # malignant = 1

  stopifnot(nrow(df) == 569,
            !anyNA(df),
            sum(df[[TARGET]] == 1) == 212,
            sum(df[[TARGET]] == 0) == 357)
  df
}


stratified_test_rows <- function(y, test_prop = TEST_PROP, seed = RANDOM_SEED) {
  set.seed(seed)
  by_class <- split(seq_along(y), y)
  test <- lapply(by_class, function(idx) {
    idx[sample.int(length(idx), round(length(idx) * test_prop))]
  })
  sort(unlist(test, use.names = FALSE))
}


get_splits <- function(scale = TRUE) {
  df <- load_data()
  test_rows <- stratified_test_rows(df[[TARGET]])

  train <- df[-test_rows, ]
  test  <- df[test_rows, ]

  X_train <- train[, FEATURES]
  X_test  <- test[, FEATURES]

  center <- NULL
  spread <- NULL
  if (scale) {
    center  <- colMeans(X_train)
    spread  <- apply(X_train, 2, sd)
    X_train <- as.data.frame(base::scale(X_train, center = center, scale = spread))
    X_test  <- as.data.frame(base::scale(X_test,  center = center, scale = spread))
  }
  rownames(X_train) <- NULL
  rownames(X_test)  <- NULL

  y_train <- train[[TARGET]]
  y_test  <- test[[TARGET]]

  list(
    X_train = X_train, X_test = X_test,
    y_train = y_train, y_test = y_test,
    train_df = cbind(X_train, diagnosis = y_train),
    test_df  = cbind(X_test,  diagnosis = y_test),
    train_id = train$id, test_id = test$id,     
    center = center, scale = spread               
  )
}


save_csvs <- function(out_dir = "data") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(load_data(), file.path(out_dir, "cleaned_full.csv"), row.names = FALSE)

  sp <- get_splits(scale = TRUE)
  write.csv(cbind(id = sp$train_id, sp$train_df),
            file.path(out_dir, "train_scaled.csv"), row.names = FALSE)
  write.csv(cbind(id = sp$test_id, sp$test_df),
            file.path(out_dir, "test_scaled.csv"), row.names = FALSE)
}


if (sys.nframe() == 0) {
  df <- load_data()
  sp <- get_splits()

  cat(sprintf("Full data: %d rows, %d features\n", nrow(df), length(FEATURES)))
  cat(sprintf("  malignant (1): %d   benign (0): %d\n",
              sum(df$diagnosis == 1), sum(df$diagnosis == 0)))
  cat(sprintf("Train: %d rows, %d malignant (%.1f%%)\n",
              length(sp$y_train), sum(sp$y_train), 100 * mean(sp$y_train)))
  cat(sprintf("Test:  %d rows, %d malignant (%.1f%%)\n",
              length(sp$y_test), sum(sp$y_test), 100 * mean(sp$y_test)))
  cat(sprintf("Scaled train: mean of column means = %.2e, column SDs all 1: %s\n",
              mean(colMeans(sp$X_train)),
              isTRUE(all.equal(unname(apply(sp$X_train, 2, sd)), rep(1, 30)))))
  save_csvs()
  cat("Wrote data/cleaned_full.csv, data/train_scaled.csv, data/test_scaled.csv\n")
}
