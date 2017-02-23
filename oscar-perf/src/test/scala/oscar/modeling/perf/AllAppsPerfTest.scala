package oscar.modeling.perf

import oscar.perf.AppPerfTest

//Todo: maybe autodetect?
class AllAppsPerfTest extends AppPerfTest (
  AllIntervals,
  AlternativeUnary,
  BACP
)