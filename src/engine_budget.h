// Global evaluation budget with interrupt and wall-time checks (spec sections 6 and 9).
#pragma once
#include <chrono>
#include <cpp11/protect.hpp>

enum StopReason { STOP_NONE = 0, STOP_BUDGET = 1, STOP_STALE = 2, STOP_TIME = 3, STOP_NO_START = 4 };

class Budget {
 public:
  Budget(double cap, double time_limit)
      : cap_(cap), used_(0.0), limit_(time_limit), next_check_(10000.0),
        stop_(STOP_NONE), start_(std::chrono::steady_clock::now()) {}

  // Spend n evaluations. Returns false, and spends nothing, once the budget or
  // the time limit is reached; the evaluation count never exceeds the cap.
  bool spend(double n) {
    if (stop_ != STOP_NONE) return false;
    if (used_ + n > cap_) { stop_ = STOP_BUDGET; return false; }
    used_ += n;
    if (used_ >= next_check_) {
      next_check_ = used_ + 10000.0;
      cpp11::check_user_interrupt();  // throws to R; no R API runs during unwinding
      if (elapsed() > limit_) { stop_ = STOP_TIME; return false; }
    }
    return true;
  }
  double elapsed() const {
    return std::chrono::duration<double>(std::chrono::steady_clock::now() - start_).count();
  }
  double used() const { return used_; }
  int stop() const { return stop_; }
  bool stopped() const { return stop_ != STOP_NONE; }

 private:
  double cap_, used_, limit_, next_check_;
  int stop_;
  std::chrono::steady_clock::time_point start_;
};
