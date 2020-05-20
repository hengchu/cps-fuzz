import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner



exp_mech([1.0,2.0,3.0])