// Global evaluation budget with interrupt and wall-time checks (spec sections 6 and 9).
#pragma once
#include <chrono>
#include <cmath>
#include <cpp11/protect.hpp>

enum StopReason { STOP_NONE = 0, STOP_BUDGET = 1, STOP_STALE = 2, STOP_TIME = 3, STOP_NO_START = 4 };

class Budget {
 public:
  // work_cap: a third limit on the recorded dense work (default none). Once the
  // accumulated work exceeds it, work() stops the run as the evaluation budget
  // does, so a run that would otherwise end on the wall clock ends at a point
  // fixed by the seed.
  Budget(double cap, double time_limit, double work_cap = INFINITY)
      : cap_(cap), used_(0.0), limit_(time_limit), next_check_(10000.0),
        work_(0.0), next_work_check_(kWorkPerCheck), work_cap_(work_cap),
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
  // Record dense linear-algebra work (about one unit per floating-point
  // multiply-add) that the evaluation count does not reflect, such as refreshes
  // and Woodbury updates at many blocks. It never touches the evaluation budget;
  // it keeps the interrupt and time checks frequent and, when a work cap is
  // set, stops the run with STOP_BUDGET once the work exceeds it. Returns false
  // once the time limit or the work cap is reached.
  bool work(double units) {
    if (stop_ != STOP_NONE) return false;
    work_ += units;
    if (work_ > work_cap_) { stop_ = STOP_BUDGET; return false; }
    if (work_ >= next_work_check_) {
      next_work_check_ = work_ + kWorkPerCheck;
      cpp11::check_user_interrupt();
      if (elapsed() > limit_) { stop_ = STOP_TIME; return false; }
    }
    return true;
  }
  double elapsed() const {
    return std::chrono::duration<double>(std::chrono::steady_clock::now() - start_).count();
  }
  double used() const { return used_; }
  double work_used() const { return work_; }
  int stop() const { return stop_; }
  bool stopped() const { return stop_ != STOP_NONE; }

 private:
  static constexpr double kWorkPerCheck = 2e7;
  double cap_, used_, limit_, next_check_, work_, next_work_check_, work_cap_;
  int stop_;
  std::chrono::steady_clock::time_point start_;
};
