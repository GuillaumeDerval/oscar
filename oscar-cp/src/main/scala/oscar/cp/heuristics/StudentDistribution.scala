package oscar.cp.heuristics

class StudentDistribution {

  // values of the student distribution for a 95% confidence interval for a two-sided test
  private[this] val distribution = Array(
    999.99,
    12.706, 4.303, 3.182, 2.776, 2.571,
    2.447, 2.365, 2.306, 2.262, 2.228,
    2.201, 2.179, 2.160, 2.145, 2.131,
    2.120, 2.110, 2.101, 2.093, 2.086,
    2.080, 2.074, 2.069, 2.064, 2.060,
    2.056, 2.052, 2.048, 2.045, 2.042,
    2.040, 2.037, 2.035, 2.032, 2.030,
    2.028, 2.026, 2.024, 2.023, 2.021,
    2.000, 1.990, 1.984, 1.980, 1.977,
    1.975, 1.973, 1.972, 1.969, 1.960
  )

  def get(index:Int):Double = {
    if(index < 41) distribution(index-1)
    else if (index < 61) distribution(40)
    else if (index < 81) distribution(41)
    else if (index < 101) distribution(42)
    else if (index < 121) distribution(43)
    else if (index < 141) distribution(44)
    else if (index < 161) distribution(45)
    else if (index < 181) distribution(46)
    else if (index < 201) distribution(47)
    else if (index < 251) distribution(48)
    else distribution(49)
  }

}
