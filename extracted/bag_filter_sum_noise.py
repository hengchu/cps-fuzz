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
  def anonymous_fun_1_(par_map_input_1_):
    """
    par_map_input_1_: Double
    """
    def anonymous_fun_2_(par_map_input_0_):
      """
      par_map_input_0_: Double
      """
      def anonymous_fun_3_(fused_input_0_):
        """
        fused_input_0_: (Double,Double)
        """
        def anonymous_fun_4_(maybeRow_1_):
          """
          maybeRow_1_: Maybe Double
          """
          if maybeRow_1_ is not None:
            cond_result_0_ = maybeRow_1_
          else:
            cond_result_0_ = 0.0
          return cond_result_0_
        def anonymous_fun_5_(row_1_):
          """
          row_1_: Double
          """
          if row_1_ < 5.0:
            cond_result_1_ = row_1_
          else:
            cond_result_1_ = None
          return cond_result_1_
        def anonymous_fun_6_(maybeRow_0_):
          """
          maybeRow_0_: Maybe Double
          """
          if maybeRow_0_ is not None:
            cond_result_2_ = maybeRow_0_
          else:
            cond_result_2_ = 0.0
          return cond_result_2_
        def anonymous_fun_7_(row_0_):
          """
          row_0_: Double
          """
          if row_0_ > 10.0:
            cond_result_3_ = row_0_
          else:
            cond_result_3_ = None
          return cond_result_3_
        return ((fun_comp(anonymous_fun_4_,anonymous_fun_5_))(fused_input_0_[0]),(fun_comp(anonymous_fun_6_,anonymous_fun_7_))(fused_input_0_[1]))
      def anonymous_fun_8_(dbrow_0_):
        """
        dbrow_0_: Double
        """
        return (dbrow_0_,dbrow_0_)
      def anonymous_fun_9_(fused_input_1_):
        """
        fused_input_1_: (Double,Double)
        """
        def anonymous_fun_10_(maybeRow_1_):
          """
          maybeRow_1_: Maybe Double
          """
          if maybeRow_1_ is not None:
            cond_result_4_ = maybeRow_1_
          else:
            cond_result_4_ = 0.0
          return cond_result_4_
        def anonymous_fun_11_(row_1_):
          """
          row_1_: Double
          """
          if row_1_ < 5.0:
            cond_result_5_ = row_1_
          else:
            cond_result_5_ = None
          return cond_result_5_
        def anonymous_fun_12_(maybeRow_0_):
          """
          maybeRow_0_: Maybe Double
          """
          if maybeRow_0_ is not None:
            cond_result_6_ = maybeRow_0_
          else:
            cond_result_6_ = 0.0
          return cond_result_6_
        def anonymous_fun_13_(row_0_):
          """
          row_0_: Double
          """
          if row_0_ > 10.0:
            cond_result_7_ = row_0_
          else:
            cond_result_7_ = None
          return cond_result_7_
        return ((fun_comp(anonymous_fun_10_,anonymous_fun_11_))(fused_input_1_[0]),(fun_comp(anonymous_fun_12_,anonymous_fun_13_))(fused_input_1_[1]))
      def anonymous_fun_14_(dbrow_1_):
        """
        dbrow_1_: Double
        """
        return (dbrow_1_,dbrow_1_)
      return ((fun_comp(anonymous_fun_3_,anonymous_fun_8_))(par_map_input_0_),(fun_comp(anonymous_fun_9_,anonymous_fun_14_))(par_map_input_0_))
    def anonymous_fun_15_(fused_input_2_):
      """
      fused_input_2_: (Double,Double)
      """
      def anonymous_fun_16_(maybeRow_1_):
        """
        maybeRow_1_: Maybe Double
        """
        if maybeRow_1_ is not None:
          cond_result_8_ = maybeRow_1_
        else:
          cond_result_8_ = 0.0
        return cond_result_8_
      def anonymous_fun_17_(row_1_):
        """
        row_1_: Double
        """
        if row_1_ < 5.0:
          cond_result_9_ = row_1_
        else:
          cond_result_9_ = None
        return cond_result_9_
      def anonymous_fun_18_(maybeRow_0_):
        """
        maybeRow_0_: Maybe Double
        """
        if maybeRow_0_ is not None:
          cond_result_10_ = maybeRow_0_
        else:
          cond_result_10_ = 0.0
        return cond_result_10_
      def anonymous_fun_19_(row_0_):
        """
        row_0_: Double
        """
        if row_0_ > 10.0:
          cond_result_11_ = row_0_
        else:
          cond_result_11_ = None
        return cond_result_11_
      return ((fun_comp(anonymous_fun_16_,anonymous_fun_17_))(fused_input_2_[0]),(fun_comp(anonymous_fun_18_,anonymous_fun_19_))(fused_input_2_[1]))
    def anonymous_fun_20_(dbrow_2_):
      """
      dbrow_2_: Double
      """
      return (dbrow_2_,dbrow_2_)
    return (anonymous_fun_2_(par_map_input_1_),(fun_comp(anonymous_fun_15_,anonymous_fun_20_))(par_map_input_1_))
  return anonymous_fun_1_
def anonymous_fun_21_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_22_(par_release_input_1_):
    """
    par_release_input_1_: (((Double,Double),(Double,Double)),(Double,Double))
    """
    def anonymous_fun_23_(par_release_input_0_):
      """
      par_release_input_0_: ((Double,Double),(Double,Double))
      """
      def anonymous_fun_24_(orange_input_0_):
        """
        orange_input_0_: (Double,Double)
        """
        return laplace_fx(cfix(1.0),orange_input_0_[0] + orange_input_0_[1])
      def anonymous_fun_25_(orange_input_1_):
        """
        orange_input_1_: (Double,Double)
        """
        return laplace_fx(cfix(2.0),orange_input_1_[0] + orange_input_1_[1])
      return (anonymous_fun_24_(par_release_input_0_[0]),anonymous_fun_25_(par_release_input_0_[1]))
    def anonymous_fun_26_(orange_input_2_):
      """
      orange_input_2_: (Double,Double)
      """
      return laplace_fx(cfix(3.0),orange_input_2_[0] + orange_input_2_[1])
    return (anonymous_fun_23_(par_release_input_1_[0]),anonymous_fun_26_(par_release_input_1_[1]))
  return anonymous_fun_22_
s123_0_ = bmcs(6,[20.0,5.0,20.0,5.0,20.0,5.0],(),anonymous_fun_0_,(),anonymous_fun_21_)
((s123_0_[0])[0] + (s123_0_[0])[1]) + s123_0_[1]