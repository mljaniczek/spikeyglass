// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Soft-thresholding operator (element-wise)
static inline mat soft_thresh(const mat& A, const mat& lam,
                              bool penalize_diagonal) {
  mat out = sign(A) % max(abs(A) - lam, zeros(size(A)));
  if (!penalize_diagonal) {
    out.diag() = A.diag();
  }
  return out;
}

// FLSA for K=2 fused penalty (Z-update step)
static void flsa2_update(const mat& A1, const mat& A2,
                         double rho, const mat& lam1, const mat& lam2,
                         mat& Z1, mat& Z2) {
  mat lam1_r = lam1 / rho;
  mat lam2_r = lam2 / rho;

  mat diff_A = A1 - A2;
  mat abs_diff = abs(diff_A);

  umat S1 = (abs_diff <= 2 * lam2_r);
  mat avg = (A1 + A2) / 2.0;
  umat S2 = (diff_A > 2 * lam2_r);
  umat S3 = ((-diff_A) > 2 * lam2_r);

  mat X_pre = conv_to<mat>::from(S1) % avg +
              conv_to<mat>::from(S2) % (A1 - lam2_r) +
              conv_to<mat>::from(S3) % (A1 + lam2_r);

  mat Y_pre = conv_to<mat>::from(S1) % avg +
              conv_to<mat>::from(S2) % (A2 + lam2_r) +
              conv_to<mat>::from(S3) % (A2 - lam2_r);

  Z1 = soft_thresh(X_pre, lam1_r, true);
  Z2 = soft_thresh(Y_pre, lam1_r, true);
}

// FLSA for general K fused penalty (Z-update step). Element-wise port of
// JGL:::flsa.general (JGL 2.3.2, (c) Patrick Danaher, MIT licence), with the
// same 1e-4 tie tolerance and the same order of operations so that results
// match the R version to rounding error.
static void flsa_general_update(const std::vector<mat>& A, int K,
                                double rho, const mat& lam1, const mat& lam2,
                                std::vector<mat>& Z) {
  const double eps = 1e-4;
  for (int k = 0; k < K; k++) Z[k].set_size(size(A[0]));

  std::vector<double> trueA(K), a(K), beta(K), newc(K);
  std::vector<int> ord(K), nord(K);
  std::vector<std::vector<int>> fus(K, std::vector<int>(K));

  for (uword e = 0; e < A[0].n_elem; e++) {
    double l2r = lam2(e) / rho;
    for (int k = 0; k < K; k++) trueA[k] = a[k] = A[k](e);
    for (int k = 0; k < K; k++) {
      newc[k] = 0.0;
      for (int o = 0; o < K; o++) {
        if (o == k) continue;
        newc[k] += (double)(a[o] - a[k] < -eps) - (double)(a[o] - a[k] > eps);
      }
      for (int kp = 0; kp < K; kp++) fus[k][kp] = 0;
    }

    for (int iter = 1; iter <= K - 1; iter++) {
      for (int k = 0; k < K; k++) {
        ord[k] = 1;
        for (int o = 0; o < K; o++) if (o != k && a[k] - a[o] > eps) ord[k]++;
        for (int o = 0; o < k; o++) if (std::abs(a[o] - a[k]) < eps) ord[k]++;
      }
      for (int k = 0; k < K; k++) beta[k] = a[k] - l2r * newc[k];
      for (int k = 0; k < K; k++) {
        nord[k] = 1;
        for (int o = 0; o < K; o++) if (o != k && beta[k] - beta[o] > eps) nord[k]++;
        for (int o = 0; o < k; o++) if (std::abs(beta[o] - beta[k]) < eps) nord[k]++;
      }
      for (int k = 0; k < K; k++) {
        for (int kp = 0; kp < K; kp++) {
          bool f = (ord[k] - 1 == ord[kp] && nord[k] < nord[kp]) ||
                   (ord[k] + 1 == ord[kp] && nord[k] > nord[kp]) ||
                   (std::abs(a[k] - a[kp]) < eps);
          if (f) fus[k][kp] = 1;
        }
      }
      // transitive closure, in the same (sequential) order as JGL
      for (int k = 0; k < K; k++) {
        for (int kp = 0; kp < K; kp++) {
          for (int o = 0; o < K; o++) {
            if (o == k || o == kp) continue;
            if (fus[k][o] && fus[kp][o]) fus[k][kp] = 1;
          }
        }
      }
      // fused groups take the mean of their (original) values
      for (int k = 0; k < K; k++) {
        double fusemean = trueA[k], denom = 1.0;
        for (int o = 0; o < K; o++) {
          if (o == k) continue;
          fusemean += fus[k][o] * trueA[o];
          denom += fus[k][o];
        }
        a[k] = fusemean / denom;
      }
      for (int k = 0; k < K; k++) {
        newc[k] = 0.0;
        for (int o = 0; o < K; o++) {
          if (o == k) continue;
          newc[k] += (double)(a[o] - a[k] < -eps) - (double)(a[o] - a[k] > eps);
        }
      }
    }

    double l1r = lam1(e) / rho;
    for (int k = 0; k < K; k++) {
      double b = a[k] - l2r * newc[k];
      double sgn = (b > 0) - (b < 0);
      Z[k](e) = sgn * std::max(std::abs(b) - l1r, 0.0);
    }
  }
}

// Group penalty Z-update (dsgl) for general K
static void dsgl_update(const std::vector<mat>& A, int K,
                        double rho, const mat& lam1, const mat& lam2,
                        std::vector<mat>& Z) {
  mat lam1_r = lam1 / rho;
  mat lam2_r = lam2 / rho;
  int p = A[0].n_rows;

  std::vector<mat> softA(K);
  for (int k = 0; k < K; k++) {
    softA[k] = soft_thresh(A[k], lam1_r, true);
  }

  mat normsoftA = zeros(p, p);
  for (int k = 0; k < K; k++) {
    normsoftA += square(softA[k]);
  }
  normsoftA = sqrt(normsoftA);

  umat notshrunk = (normsoftA > lam2_r);
  mat normsoftA_safe = normsoftA + conv_to<mat>::from(1 - notshrunk);

  for (int k = 0; k < K; k++) {
    Z[k] = softA[k] % (1.0 - lam2_r / normsoftA_safe);
    Z[k] %= conv_to<mat>::from(notshrunk);
  }
}

// ADMM solver that accepts precomputed S (avoids redundant cov() calls)
// warm_ptr: optional warm-start for theta (precision matrices)
static List admm_with_S(const std::vector<mat>& S, const vec& n,
                         int p, int K,
                         const mat& lam1, const mat& lam2,
                         const std::string& penalty, double rho,
                         double rho_increment, int maxiter, double tol,
                         const std::vector<mat>* warm_ptr) {
  // Initialize theta
  std::vector<mat> theta(K);
  if (warm_ptr != nullptr) {
    for (int k = 0; k < K; k++) {
      theta[k] = (*warm_ptr)[k];
    }
  } else {
    for (int k = 0; k < K; k++) {
      vec d = S[k].diag();
      double min_pos = datum::inf;
      for (unsigned i = 0; i < d.n_elem; i++) {
        if (d(i) > 0 && d(i) < min_pos) min_pos = d(i);
      }
      for (unsigned i = 0; i < d.n_elem; i++) {
        if (d(i) == 0) d(i) = min_pos / 2.0;
      }
      theta[k] = diagmat(1.0 / d);
    }
  }

  std::vector<mat> Z(K), W(K);
  for (int k = 0; k < K; k++) {
    Z[k] = zeros(p, p);
    W[k] = zeros(p, p);
  }

  int iter = 0;
  double diff_value = 10.0;
  double current_rho = rho;
  std::vector<mat> theta_prev(K);

  while ((iter == 0) || (iter < maxiter && diff_value > tol)) {
    for (int k = 0; k < K; k++) {
      theta_prev[k] = theta[k];
    }

    // Theta update via symmetric eigendecomposition
    for (int k = 0; k < K; k++) {
      mat M = S[k] - (current_rho / n(k)) * Z[k] + (current_rho / n(k)) * W[k];
      M = 0.5 * (M + M.t());

      vec D;
      mat V;
      eig_sym(D, V, M);

      vec D2 = (n(k) / (2.0 * current_rho)) *
               (-D + sqrt(square(D) + 4.0 * current_rho / n(k)));
      theta[k] = V * diagmat(D2) * V.t();
    }

    // Z update
    std::vector<mat> A(K);
    for (int k = 0; k < K; k++) {
      A[k] = theta[k] + W[k];
    }

    if (penalty == "fused") {
      if (K == 2) {
        flsa2_update(A[0], A[1], current_rho, lam1, lam2, Z[0], Z[1]);
      } else {
        flsa_general_update(A, K, current_rho, lam1, lam2, Z);
      }
    } else {
      dsgl_update(A, K, current_rho, lam1, lam2, Z);
    }

    // W update
    for (int k = 0; k < K; k++) {
      W[k] += theta[k] - Z[k];
    }

    iter++;
    diff_value = 0.0;
    for (int k = 0; k < K; k++) {
      double num = accu(abs(theta[k] - theta_prev[k]));
      double den = accu(abs(theta_prev[k]));
      if (den > 0) {
        diff_value += num / den;
      }
    }
    current_rho *= rho_increment;
  }

  double diff_final = 0.0;
  for (int k = 0; k < K; k++) {
    diff_final += accu(abs(theta[k] - Z[k]));
  }

  List theta_out(K), Z_out(K);
  for (int k = 0; k < K; k++) {
    theta_out[k] = wrap(theta[k]);
    Z_out[k] = wrap(Z[k]);
  }

  return List::create(
    Named("theta") = theta_out,
    Named("Z") = Z_out,
    Named("diff") = diff_final,
    Named("iters") = iter
  );
}


//' ADMM iterations with adaptive penalties (C++ implementation)
//'
//' @param S_list List of K (biased) sample covariance matrices (p x p). With
//'   missing data these are the expected covariances E(S given observed data).
//' @param lam1 Matrix-valued lambda1 penalty (p x p)
//' @param lam2 Matrix-valued lambda2 penalty (p x p)
//' @param penalty String: "fused" or "group"
//' @param rho ADMM step size
//' @param rho_increment Multiplicative rho update factor
//' @param weights Numeric vector of length K (sample sizes)
//' @param maxiter Maximum ADMM iterations
//' @param tol Convergence tolerance
//' @param warm_list Optional list of K warm-start precision matrices (or R_NilValue)
//' @return List with theta (list of K matrices), Z (list of K matrices), diff, iters
//' @keywords internal
// [[Rcpp::export]]
List admm_iters_adaptive_cpp(List S_list, arma::mat lam1, arma::mat lam2,
                             std::string penalty, double rho,
                             double rho_increment, NumericVector weights,
                             int maxiter, double tol,
                             Nullable<List> warm_list = R_NilValue) {
  int K = S_list.size();
  std::vector<mat> S(K);
  vec n(K);
  for (int k = 0; k < K; k++) {
    S[k] = as<mat>(S_list[k]);
    n(k) = weights[k];
  }
  int p = S[0].n_rows;

  std::vector<mat>* warm_ptr = nullptr;
  std::vector<mat> warm_mats;
  if (warm_list.isNotNull()) {
    List warm = as<List>(warm_list);
    warm_mats.resize(K);
    for (int k = 0; k < K; k++) {
      warm_mats[k] = as<mat>(warm[k]);
    }
    warm_ptr = &warm_mats;
  }

  return admm_with_S(S, n, p, K, lam1, lam2, penalty, rho,
                     rho_increment, maxiter, tol, warm_ptr);
}


// Log of the Laplace prior kernel (lambda/v)^cnt * exp(-lambda/v * x).
// With lambda = 0 the penalty is absent, so the term is dropped rather than
// giving log(0) = -Inf in every mixture component.
static inline mat laplace_logprior(double lambda, double v, const mat& x,
                                   double cnt) {
  if (lambda == 0.0) return zeros(size(x));
  return -lambda / v * x + std::log(lambda / v) * cnt;
}

// E-step: compute posterior inclusion probabilities and adaptive penalty weights
// Single spike-and-slab (not doubly)
static void gete_cpp(int p, const std::vector<mat>& theta,
                     double lambda1, double lambda2,
                     double v0, double v1, double pi_delta,
                     const std::string& penalty,
                     mat& d1_out, mat& prob1_out) {
  int G = theta.size();
  mat pen = zeros(p, p);

  if (penalty == "fused") {
    for (int i = 0; i < G - 1; i++) {
      for (int j = i + 1; j < G; j++) {
        pen += abs(theta[i] - theta[j]);
      }
    }
  } else {
    // group
    for (int i = 0; i < G; i++) {
      pen += square(theta[i]);
    }
    pen = sqrt(pen);
  }

  double fused_const = (penalty == "fused") ? G * (G - 1) / 2.0 : 1.0;

  mat abssum = zeros(p, p);
  for (int i = 0; i < G; i++) {
    abssum += abs(theta[i]);
  }

  mat logp0 = laplace_logprior(lambda1, v0, abssum, G)
              + laplace_logprior(lambda2, v0, pen, fused_const);
  mat logp1 = laplace_logprior(lambda1, v1, abssum, G)
              + laplace_logprior(lambda2, v1, pen, fused_const);

  // prob = exp(logp1) * pi / (exp(logp0) * (1-pi) + exp(logp1) * pi)
  // Use log-space for numerical stability
  mat log_num = logp1 + std::log(pi_delta);
  mat log_den0 = logp0 + std::log(1.0 - pi_delta);

  // log-sum-exp for denominator
  mat mx = max(log_num, log_den0);
  mat log_den = mx + log(exp(log_num - mx) + exp(log_den0 - mx));
  prob1_out = exp(log_num - log_den);

  // Handle NaN
  prob1_out.replace(datum::nan, 0.0);

  d1_out = (1.0 - prob1_out) / v0 + prob1_out / v1;
  d1_out.replace(datum::nan, 1.0 / v0);

  prob1_out.diag().zeros();
  d1_out.diag().zeros();

  // Symmetrize
  prob1_out = 0.5 * (prob1_out + prob1_out.t());
  d1_out = 0.5 * (d1_out + d1_out.t());
}

// E-step: doubly spike-and-slab
static void gete_doubly_cpp(int p, const std::vector<mat>& theta,
                            double lambda1, double lambda2,
                            double v0, double v1,
                            double pi_delta, double pi_xi,
                            const std::string& penalty,
                            mat& d1_out, mat& prob1_out,
                            mat& d2_out, mat& prob2_out) {
  int G = theta.size();
  mat pen = zeros(p, p);

  if (penalty == "fused") {
    for (int i = 0; i < G - 1; i++) {
      for (int j = i + 1; j < G; j++) {
        pen += abs(theta[i] - theta[j]);
      }
    }
  } else {
    for (int i = 0; i < G; i++) {
      pen += square(theta[i]);
    }
    pen = sqrt(pen);
  }

  double fused_const = (penalty == "fused") ? G * (G - 1) / 2.0 : 1.0;

  mat abssum = zeros(p, p);
  for (int i = 0; i < G; i++) {
    abssum += abs(theta[i]);
  }

  // Three log-probabilities: (0,0), (1,0), (1,1)
  mat lp00 = laplace_logprior(lambda1, v0, abssum, G)
             + laplace_logprior(lambda2, v0, pen, fused_const)
             + std::log(1.0 - pi_delta) + std::log(1.0 - pi_xi);
  mat lp10 = laplace_logprior(lambda1, v1, abssum, G)
             + laplace_logprior(lambda2, v0, pen, fused_const)
             + std::log(pi_delta) + std::log(1.0 - pi_xi);
  mat lp11 = laplace_logprior(lambda1, v1, abssum, G)
             + laplace_logprior(lambda2, v1, pen, fused_const)
             + std::log(pi_delta) + std::log(pi_xi);

  // Log-sum-exp normalization
  mat mx = max(max(lp00, lp10), lp11);
  mat logz = mx + log(exp(lp00 - mx) + exp(lp10 - mx) + exp(lp11 - mx));

  mat p00 = exp(lp00 - logz);
  mat p10 = exp(lp10 - logz);
  mat p11 = exp(lp11 - logz);

  prob1_out = p10 + p11;  // P(delta=1)
  prob2_out = p11;         // P(xi=1)

  prob1_out.replace(datum::nan, 0.0);
  prob2_out.replace(datum::nan, 0.0);

  d1_out = (1.0 - prob1_out) / v0 + prob1_out / v1;
  d2_out = (1.0 - prob2_out) / v0 + prob2_out / v1;
  d1_out.replace(datum::nan, 1.0 / v0);
  d2_out.replace(datum::nan, 1.0 / v0);

  prob1_out.diag().zeros();
  prob2_out.diag().zeros();
  d1_out.diag().zeros();
  d2_out.diag().zeros();

  prob1_out = 0.5 * (prob1_out + prob1_out.t());
  prob2_out = 0.5 * (prob2_out + prob2_out.t());
  d1_out = 0.5 * (d1_out + d1_out.t());
  d2_out = 0.5 * (d2_out + d2_out.t());
}


// Connected components via BFS on a symmetric boolean matrix
// Returns component labels (0-indexed) and number of components
static void find_components(const umat& adj, int p,
                            ivec& labels, int& n_components) {
  labels.set_size(p);
  labels.fill(-1);
  n_components = 0;

  for (int start = 0; start < p; start++) {
    if (labels(start) >= 0) continue;

    // BFS from this node
    labels(start) = n_components;
    std::vector<int> queue;
    queue.push_back(start);
    int head = 0;

    while (head < (int)queue.size()) {
      int node = queue[head++];
      for (int j = 0; j < p; j++) {
        if (j != node && adj(node, j) && labels(j) < 0) {
          labels(j) = n_components;
          queue.push_back(j);
        }
      }
    }
    n_components++;
  }
}


//' Fused-lasso Z-update for general K (internal; exposed for testing)
//'
//' @param A_list List of K p x p matrices
//' @param rho ADMM step size
//' @param lam1 Matrix-valued lambda1 penalty (p x p)
//' @param lam2 Matrix-valued lambda2 penalty (p x p)
//' @return List of K matrices
//' @keywords internal
// [[Rcpp::export]]
List flsa_general_cpp(List A_list, double rho, arma::mat lam1, arma::mat lam2) {
  int K = A_list.size();
  std::vector<mat> A(K), Z(K);
  for (int k = 0; k < K; k++) A[k] = as<mat>(A_list[k]);
  flsa_general_update(A, K, rho, lam1, lam2, Z);
  List out(K);
  for (int k = 0; k < K; k++) out[k] = wrap(Z[k]);
  return out;
}


//' Connected components of an undirected graph (internal)
//'
//' Breadth-first search on the symmetrised adjacency matrix; replaces
//' igraph::components() (weak connectivity) in the R EM path.
//'
//' @param adj Logical p x p adjacency matrix
//' @return Integer vector of 1-based component labels, numbered in order of
//'   each component's lowest-index node
//' @keywords internal
// [[Rcpp::export]]
IntegerVector connected_components_cpp(LogicalMatrix adj) {
  int p = adj.nrow();
  umat a(p, p, fill::zeros);
  for (int i = 0; i < p; i++) {
    for (int j = 0; j < p; j++) {
      if (adj(i, j) == TRUE || adj(j, i) == TRUE) a(i, j) = 1;
    }
  }
  ivec labels;
  int n_comp;
  find_components(a, p, labels, n_comp);
  IntegerVector out(p);
  for (int i = 0; i < p; i++) out[i] = labels(i) + 1;
  return out;
}


//' Full EM inner loop in C++ (E-step + block detection + ADMM)
//'
//' Runs the EM algorithm for a single v0 step entirely in C++, avoiding
//' R overhead between iterations. Handles block decomposition via connected
//' components (replacing igraph), precomputes covariance once, and runs
//' the ADMM solver per block.
//'
//' @param S_list List of K sample covariance matrices (p x p, biased)
//' @param addvar_list List of K addvar matrices for imputation (or NULL)
//' @param n_vec Numeric vector of sample sizes (length K)
//' @param theta_init List of K initial precision matrices
//' @param penalty String: "fused" or "group"
//' @param lambda0 Scalar diagonal penalty
//' @param lambda1 Scalar off-diagonal penalty
//' @param lambda2 Scalar cross-group penalty
//' @param v0 Spike variance for this step
//' @param v1 Slab variance
//' @param doubly Logical: use doubly spike-and-slab?
//' @param rho ADMM step size
//' @param a Beta prior shape1
//' @param b Beta prior shape2
//' @param maxitr_em Max EM iterations
//' @param tol_em EM convergence tolerance
//' @param maxitr_jgl Max ADMM iterations
//' @param tol_jgl ADMM convergence tolerance
//' @param truncate Threshold for zeroing small entries
//' @param pi_delta_init,pi_xi_init Starting values of pi_delta and pi_xi
//' @param pi_floor pi is kept between pi_floor and 1 - pi_floor
//' @param tol_pi Convergence tolerance for the change in log(pi)
//' @return List with theta, prob1, prob2, d1, d2, pi_delta, pi_xi, itr
//' @keywords internal
// [[Rcpp::export]]
List ssjgl_em_inner_cpp(List S_list, Nullable<List> addvar_list,
                        NumericVector n_vec,
                        List theta_init,
                        std::string penalty,
                        double lambda0, double lambda1, double lambda2,
                        double v0, double v1,
                        bool doubly,
                        double rho, double a, double b,
                        int maxitr_em, double tol_em,
                        int maxitr_jgl, double tol_jgl,
                        double truncate,
                        double pi_delta_init, double pi_xi_init,
                        double pi_floor, double tol_pi) {
  int K = S_list.size();
  mat S0 = as<mat>(S_list[0]);
  int p = S0.n_rows;

  vec n(K);
  for (int k = 0; k < K; k++) {
    n(k) = n_vec[k];
  }

  // Load initial theta
  std::vector<mat> theta(K);
  for (int k = 0; k < K; k++) {
    theta[k] = as<mat>(theta_init[k]);
  }

  // Load S matrices (may include addvar adjustment)
  std::vector<mat> S_base(K);
  for (int k = 0; k < K; k++) {
    S_base[k] = as<mat>(S_list[k]);
  }

  // Initialize E-step outputs
  mat d1 = ones(p, p);
  mat d2 = ones(p, p);
  mat prob1(p, p, fill::value(a / (a + b)));
  mat prob2(p, p, fill::value(a / (a + b)));
  d1.diag().zeros();
  d2.diag().zeros();
  prob1.diag().zeros();
  prob2.diag().zeros();

  double pi_delta = pi_delta_init;
  double pi_xi = pi_xi_init;
  double pi_delta_last = pi_delta, pi_xi_last = pi_xi;
  bool pi_delta_initialized = false;
  // Converged when theta changes by < tol_em AND log(pi) by < tol_pi
  double diff = 1.0, diff_pi = 1.0;
  int itr = 0;
  double n_edges = p * (p - 1.0) / 2.0;

  std::vector<mat> theta_last(K);

  for (itr = 1; itr <= maxitr_em; itr++) {
    if (diff < tol_em && diff_pi < tol_pi) break;

    // Compute S with addvar if present
    std::vector<mat> S(K);
    if (addvar_list.isNotNull()) {
      List av = as<List>(addvar_list);
      for (int k = 0; k < K; k++) {
        mat addvar_k = as<mat>(av[k]);
        S[k] = S_base[k] + addvar_k / n(k);
      }
    } else {
      S = S_base;
    }

    // E-step
    if (doubly) {
      gete_doubly_cpp(p, theta, lambda1, lambda2, v0, v1,
                      pi_delta, pi_xi, penalty,
                      d1, prob1, d2, prob2);
    } else {
      gete_cpp(p, theta, lambda1, lambda2, v0, v1, pi_delta, penalty,
               d1, prob1);
      d2.zeros();
      prob2.zeros();
    }

    // Pi update
    // Beta posterior mode, kept inside [pi_floor, 1 - pi_floor] so that
    // log(pi) stays finite in the next E-step (pi_xi could otherwise reach
    // exactly 0 and stay there)
    pi_delta = (a + accu(prob1) / 2.0 - 1.0) / (a + b + n_edges - 2.0);
    pi_delta = std::min(std::max(pi_delta, pi_floor), 1.0 - pi_floor);
    if (doubly) {
      pi_xi = (a + accu(prob2) / 2.0 - 1.0) / (a + b + n_edges - 2.0);
      pi_xi = std::min(std::max(pi_xi, pi_floor), 1.0 - pi_floor);
    }

    // Compute penalty weight matrices
    mat lam1_current = lambda1 * d1;
    mat lam2_current;
    if (doubly) {
      lam2_current = lambda2 * d2;
    } else {
      lam2_current = lambda2 * d1;
    }

    // === Block detection (replaces igraph) ===
    // Compute screening criterion
    umat critboth;
    if (penalty == "fused") {
      if (K == 2) {
        // Bi-conditional screening rule
        umat crit1_0 = (abs(S[0]) * n(0) > lam1_current + lam2_current);
        umat crit1_1 = (abs(S[1]) * n(1) > lam1_current + lam2_current);
        mat S_sum = n(0) * S[0] + n(1) * S[1];
        umat crit2 = (abs(S_sum) > 2 * lam1_current);
        critboth = crit2 + crit1_0 + crit1_1;
      } else {
        umat crit_sum(p, p, fill::zeros);
        for (int k = 0; k < K; k++) {
          crit_sum += (abs(S[k]) * n(k) > lam1_current);
        }
        critboth = crit_sum;
      }
    } else {
      // Group penalty screening
      mat tempsum = zeros(p, p);
      for (int k = 0; k < K; k++) {
        mat term = max(n(k) * abs(S[k]) - lam1_current, zeros(p, p));
        tempsum += square(term);
      }
      critboth = (tempsum > square(lam2_current));
    }

    // Set diagonal to 1 (always connected to self)
    critboth.diag().ones();
    // Convert to boolean
    umat adj = (critboth > 0);

    // Find connected components
    ivec comp_labels;
    int n_comp;
    find_components(adj, p, comp_labels, n_comp);

    // Classify: singletons (unconnected) vs blocks
    std::vector<std::vector<int>> blocks;
    std::vector<int> singletons;
    for (int c = 0; c < n_comp; c++) {
      std::vector<int> members;
      for (int j = 0; j < p; j++) {
        if (comp_labels(j) == c) members.push_back(j);
      }
      if (members.size() == 1) {
        singletons.push_back(members[0]);
      } else {
        blocks.push_back(members);
      }
    }

    // Initialize result theta to zero
    std::vector<mat> theta_new(K);
    for (int k = 0; k < K; k++) {
      theta_new[k] = zeros(p, p);
    }

    // Handle singletons: diagonal = 1/(S_kk + lambda0/n_k)
    for (int idx : singletons) {
      for (int k = 0; k < K; k++) {
        double s_diag = S[k](idx, idx);
        // Closed form theta = 1/(s + lambda0/n_k) for unconnected nodes:
        // the fixed point of JGL:::admm.iters.unconnected for scalars
        if (s_diag + lambda0 > 0) {
          theta_new[k](idx, idx) = 1.0 / (s_diag + lambda0 / n(k));
        }
      }
    }

    // Handle blocks: run ADMM on each block
    for (const auto& bl : blocks) {
      int bp = bl.size();
      uvec bl_idx(bp);
      for (int i = 0; i < bp; i++) bl_idx(i) = bl[i];

      // Extract block of S
      std::vector<mat> S_bl(K);
      vec n_bl = n;
      for (int k = 0; k < K; k++) {
        S_bl[k] = S[k](bl_idx, bl_idx);
      }

      // Extract block of penalty matrices
      mat lam1_bl = lam1_current(bl_idx, bl_idx);
      // Set diagonal to lambda0
      lam1_bl.diag().fill(lambda0);
      mat lam2_bl = lam2_current(bl_idx, bl_idx);
      lam2_bl.diag().zeros();

      // Run ADMM on this block
      List result_bl = admm_with_S(S_bl, n_bl, bp, K,
                                    lam1_bl, lam2_bl, penalty,
                                    rho, 1.0, maxitr_jgl, tol_jgl, nullptr);

      // Extract Z (the consensus variable = final estimate)
      List Z_bl = result_bl["Z"];
      for (int k = 0; k < K; k++) {
        mat Zk = as<mat>(Z_bl[k]);
        // Place block result into full theta
        for (int i = 0; i < bp; i++) {
          for (int j = 0; j < bp; j++) {
            theta_new[k](bl[i], bl[j]) = Zk(i, j);
          }
        }
      }
    }

    // Truncate small entries
    for (int k = 0; k < K; k++) {
      umat small = (abs(theta_new[k]) < truncate);
      small.diag().zeros();  // never truncate diagonal
      theta_new[k] %= conv_to<mat>::from(1 - small);
    }

    // Convergence check
    if (pi_delta_initialized) {
      diff = 0.0;
      for (int k = 0; k < K; k++) {
        diff = std::max(diff, (double)max(max(abs(theta_last[k] - theta_new[k]))));
      }
      diff_pi = std::abs(std::log(pi_delta) - std::log(pi_delta_last));
      if (doubly) {
        diff_pi = std::max(diff_pi, std::abs(std::log(pi_xi) - std::log(pi_xi_last)));
      }
    }
    pi_delta_initialized = true;
    pi_delta_last = pi_delta;
    pi_xi_last = pi_xi;
    theta_last = theta_new;
    theta = theta_new;
  }

  // Package output
  List theta_out(K);
  for (int k = 0; k < K; k++) {
    theta_out[k] = wrap(theta[k]);
  }

  return List::create(
    Named("theta") = theta_out,
    Named("prob1") = wrap(prob1),
    Named("prob2") = wrap(prob2),
    Named("d1") = wrap(d1),
    Named("d2") = wrap(d2),
    Named("pi_delta") = pi_delta,
    Named("pi_xi") = pi_xi,
    Named("itr") = itr
  );
}
