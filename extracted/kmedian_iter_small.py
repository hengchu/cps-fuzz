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
  def anonymous_fun_1_(fused_input_2_):
    """
    fused_input_2_: ((((Double,Double),(Double,Double)),(Double,Double)),(Double,Double))
    """
    def anonymous_fun_2_(fused_input_1_):
      """
      fused_input_1_: (((Double,Double),(Double,Double)),(Double,Double))
      """
      def anonymous_fun_3_(fused_input_0_):
        """
        fused_input_0_: ((Double,Double),(Double,Double))
        """
        def anonymous_fun_4_(row_3_):
          """
          row_3_: (Double,Double)
          """
          return ((0.1,0.1)[0] - row_3_[0]) * ((0.1,0.1)[0] - row_3_[0]) + ((0.1,0.1)[1] - row_3_[1]) * ((0.1,0.1)[1] - row_3_[1])
        def anonymous_fun_5_(row_2_):
          """
          row_2_: (Double,Double)
          """
          return ((0.1,0.0)[0] - row_2_[0]) * ((0.1,0.0)[0] - row_2_[0]) + ((0.1,0.0)[1] - row_2_[1]) * ((0.1,0.0)[1] - row_2_[1])
        return (anonymous_fun_4_(fused_input_0_[0]),anonymous_fun_5_(fused_input_0_[1]))
      def anonymous_fun_6_(row_1_):
        """
        row_1_: (Double,Double)
        """
        return ((0.0,0.1)[0] - row_1_[0]) * ((0.0,0.1)[0] - row_1_[0]) + ((0.0,0.1)[1] - row_1_[1]) * ((0.0,0.1)[1] - row_1_[1])
      return (anonymous_fun_3_(fused_input_1_[0]),anonymous_fun_6_(fused_input_1_[1]))
    def anonymous_fun_7_(row_0_):
      """
      row_0_: (Double,Double)
      """
      return ((0.0,0.0)[0] - row_0_[0]) * ((0.0,0.0)[0] - row_0_[0]) + ((0.0,0.0)[1] - row_0_[1]) * ((0.0,0.0)[1] - row_0_[1])
    return (anonymous_fun_2_(fused_input_2_[0]),anonymous_fun_7_(fused_input_2_[1]))
  def anonymous_fun_8_(dbrow_0_):
    """
    dbrow_0_: (Double,Double)
    """
    return (((dbrow_0_,dbrow_0_),dbrow_0_),dbrow_0_)
  return fun_comp(anonymous_fun_1_,anonymous_fun_8_)
def anonymous_fun_9_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_10_(orange_input_0_):
    """
    orange_input_0_: (((Double,Double),Double),Double)
    """
    return exp_mech_fx([((orange_input_0_[0])[0])[0],((orange_input_0_[0])[0])[1],(orange_input_0_[0])[1],orange_input_0_[1]])
  return anonymous_fun_10_
selected_pair_0_ = bmcs(4,[1.0,1.0,1.0,1.0],(),anonymous_fun_0_,(),anonymous_fun_9_)
([(0.1,0.2)][0:([(0,0),(0,1),(0,2),(0,3)][selected_pair_0_])[0]] + [[(0.0,0.0),(0.0,0.1),(0.1,0.0),(0.1,0.1)][([(0,0),(0,1),(0,2),(0,3)][selected_pair_0_])[1]]]) + [(0.1,0.2)][([(0,0),(0,1),(0,2),(0,3)][selected_pair_0_])[0] + 1:1]