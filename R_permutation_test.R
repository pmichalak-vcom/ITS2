> # Data
> scram <- c(0.290796, 0.4222384, 0.3818306, 0.7059225)
> siRNA <- c(0.1959897, 0.4335755, 0.3649097, 0.2008358)
> mean_diff <- function(x, y) {
+   mean(x) - mean(y)
+ }
> observed_diff <- mean(scram) - mean(siRNA)
> combined_data <- c(scram, siRNA)
> num_permutations <- 10000
> permuted_diffs <- numeric(num_permutations)
> set.seed(123)  # for reproducibility
> # Permutation test
> set.seed(123)  # for reproducibility
> for (i in 1:num_permutations) {
+   # Permute the combined data
+   permuted_data <- sample(combined_data)
+   # Calculate mean difference for permuted data
+   permuted_diffs[i] <- mean(permuted_data[1:4]) - mean(permuted_data[5:8])
+ }
> p_value_perm <- mean(permuted_diffs >= observed_diff) / 2
> cat("Observed Mean Difference:", observed_diff, "\n")
> cat("Permutation Test p-value:", p_value_perm, "\n")
