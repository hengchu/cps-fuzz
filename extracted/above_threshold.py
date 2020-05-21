import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  """
  empty_closure_0_: ()
  """
  def anonymous_fun_1_(row_0_):
    """
    row_0_: Double
    """
    return row_0_
  def anonymous_fun_2_(dbrow_0_):
    """
    dbrow_0_: Double
    """
    return dbrow_0_
  return fun_comp(anonymous_fun_1_,anonymous_fun_2_)
def anonymous_fun_3_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_4_(orange_input_0_):
    """
    orange_input_0_: Double
    """
    above_thresh_result_0_ = laplace_fx(1.0,orange_input_0_)
    above_thresh_result_0_ = MemValue(cfix(above_thresh_result_0_))
    def above_thresh_true_fun_0_():
      skip
    def above_thresh_false_fun_0_():
      above_thresh_result_0_.write(cfix(10.0))
    if_statement(above_thresh_result_0_ - cfix(10.0) > cfix(1.0),above_thresh_true_fun_0_,above_thresh_false_fun_0_)
    above_thresh_result_0_ = above_thresh_result_0_.read()
    return above_thresh_result_0_
  return anonymous_fun_4_
bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)