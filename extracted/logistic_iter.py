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
    row_0_: Vec Double
    """
    return 1.0
  def anonymous_fun_2_(dbrow_0_):
    """
    dbrow_0_: Vec Double
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
    return laplace_fx(cfix(1.0),orange_input_0_)
  return anonymous_fun_4_
db_size_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
def anonymous_fun_5_(empty_closure_2_):
  """
  empty_closure_2_: ()
  """
  def anonymous_fun_6_(par_map_input_3_):
    """
    par_map_input_3_: Vec Double
    """
    def anonymous_fun_7_(g_4_):
      """
      g_4_: Vec Double
      """
      return g_4_[0]
    def anonymous_fun_8_(row_1_):
      """
      row_1_: Vec Double
      """
      def anonymous_fun_9_(curr_acc_0_):
        """
        curr_acc_0_: Vec Double
        """
        return len(curr_acc_0_) < len(row_1_)
      def anonymous_fun_10_(curr_acc_1_):
        """
        curr_acc_1_: Vec Double
        """
        def anonymous_fun_11_(curr_acc_2_):
          """
          curr_acc_2_: (Int,Double)
          """
          return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
        def anonymous_fun_12_(curr_acc_3_):
          """
          curr_acc_3_: (Int,Double)
          """
          return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
        loop_acc_1_ = (0,0.0)
        while anonymous_fun_11_(loop_acc_1_):
          loop_acc_1_ = anonymous_fun_12_(loop_acc_1_)
        return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_1_[1])))) * row_1_[len(curr_acc_1_)]]
      loop_acc_0_ = []
      while anonymous_fun_9_(loop_acc_0_):
        loop_acc_0_ = anonymous_fun_10_(loop_acc_0_)
      return loop_acc_0_
    def anonymous_fun_13_(dbrow_1_):
      """
      dbrow_1_: Vec Double
      """
      return dbrow_1_
    def anonymous_fun_14_(par_map_input_2_):
      """
      par_map_input_2_: Vec Double
      """
      def anonymous_fun_15_(g_3_):
        """
        g_3_: Vec Double
        """
        return g_3_[1]
      def anonymous_fun_16_(row_1_):
        """
        row_1_: Vec Double
        """
        def anonymous_fun_17_(curr_acc_0_):
          """
          curr_acc_0_: Vec Double
          """
          return len(curr_acc_0_) < len(row_1_)
        def anonymous_fun_18_(curr_acc_1_):
          """
          curr_acc_1_: Vec Double
          """
          def anonymous_fun_19_(curr_acc_2_):
            """
            curr_acc_2_: (Int,Double)
            """
            return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
          def anonymous_fun_20_(curr_acc_3_):
            """
            curr_acc_3_: (Int,Double)
            """
            return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
          loop_acc_3_ = (0,0.0)
          while anonymous_fun_19_(loop_acc_3_):
            loop_acc_3_ = anonymous_fun_20_(loop_acc_3_)
          return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_3_[1])))) * row_1_[len(curr_acc_1_)]]
        loop_acc_2_ = []
        while anonymous_fun_17_(loop_acc_2_):
          loop_acc_2_ = anonymous_fun_18_(loop_acc_2_)
        return loop_acc_2_
      def anonymous_fun_21_(dbrow_2_):
        """
        dbrow_2_: Vec Double
        """
        return dbrow_2_
      def anonymous_fun_22_(par_map_input_1_):
        """
        par_map_input_1_: Vec Double
        """
        def anonymous_fun_23_(g_2_):
          """
          g_2_: Vec Double
          """
          return g_2_[2]
        def anonymous_fun_24_(row_1_):
          """
          row_1_: Vec Double
          """
          def anonymous_fun_25_(curr_acc_0_):
            """
            curr_acc_0_: Vec Double
            """
            return len(curr_acc_0_) < len(row_1_)
          def anonymous_fun_26_(curr_acc_1_):
            """
            curr_acc_1_: Vec Double
            """
            def anonymous_fun_27_(curr_acc_2_):
              """
              curr_acc_2_: (Int,Double)
              """
              return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
            def anonymous_fun_28_(curr_acc_3_):
              """
              curr_acc_3_: (Int,Double)
              """
              return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
            loop_acc_5_ = (0,0.0)
            while anonymous_fun_27_(loop_acc_5_):
              loop_acc_5_ = anonymous_fun_28_(loop_acc_5_)
            return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_5_[1])))) * row_1_[len(curr_acc_1_)]]
          loop_acc_4_ = []
          while anonymous_fun_25_(loop_acc_4_):
            loop_acc_4_ = anonymous_fun_26_(loop_acc_4_)
          return loop_acc_4_
        def anonymous_fun_29_(dbrow_3_):
          """
          dbrow_3_: Vec Double
          """
          return dbrow_3_
        def anonymous_fun_30_(par_map_input_0_):
          """
          par_map_input_0_: Vec Double
          """
          def anonymous_fun_31_(g_1_):
            """
            g_1_: Vec Double
            """
            return g_1_[3]
          def anonymous_fun_32_(row_1_):
            """
            row_1_: Vec Double
            """
            def anonymous_fun_33_(curr_acc_0_):
              """
              curr_acc_0_: Vec Double
              """
              return len(curr_acc_0_) < len(row_1_)
            def anonymous_fun_34_(curr_acc_1_):
              """
              curr_acc_1_: Vec Double
              """
              def anonymous_fun_35_(curr_acc_2_):
                """
                curr_acc_2_: (Int,Double)
                """
                return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
              def anonymous_fun_36_(curr_acc_3_):
                """
                curr_acc_3_: (Int,Double)
                """
                return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
              loop_acc_7_ = (0,0.0)
              while anonymous_fun_35_(loop_acc_7_):
                loop_acc_7_ = anonymous_fun_36_(loop_acc_7_)
              return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_7_[1])))) * row_1_[len(curr_acc_1_)]]
            loop_acc_6_ = []
            while anonymous_fun_33_(loop_acc_6_):
              loop_acc_6_ = anonymous_fun_34_(loop_acc_6_)
            return loop_acc_6_
          def anonymous_fun_37_(dbrow_4_):
            """
            dbrow_4_: Vec Double
            """
            return dbrow_4_
          def anonymous_fun_38_(g_0_):
            """
            g_0_: Vec Double
            """
            return g_0_[4]
          def anonymous_fun_39_(row_1_):
            """
            row_1_: Vec Double
            """
            def anonymous_fun_40_(curr_acc_0_):
              """
              curr_acc_0_: Vec Double
              """
              return len(curr_acc_0_) < len(row_1_)
            def anonymous_fun_41_(curr_acc_1_):
              """
              curr_acc_1_: Vec Double
              """
              def anonymous_fun_42_(curr_acc_2_):
                """
                curr_acc_2_: (Int,Double)
                """
                return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
              def anonymous_fun_43_(curr_acc_3_):
                """
                curr_acc_3_: (Int,Double)
                """
                return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
              loop_acc_9_ = (0,0.0)
              while anonymous_fun_42_(loop_acc_9_):
                loop_acc_9_ = anonymous_fun_43_(loop_acc_9_)
              return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_9_[1])))) * row_1_[len(curr_acc_1_)]]
            loop_acc_8_ = []
            while anonymous_fun_40_(loop_acc_8_):
              loop_acc_8_ = anonymous_fun_41_(loop_acc_8_)
            return loop_acc_8_
          def anonymous_fun_44_(dbrow_5_):
            """
            dbrow_5_: Vec Double
            """
            return dbrow_5_
          return ((fun_comp(fun_comp(anonymous_fun_31_,anonymous_fun_32_),anonymous_fun_37_))(par_map_input_0_),(fun_comp(fun_comp(anonymous_fun_38_,anonymous_fun_39_),anonymous_fun_44_))(par_map_input_0_))
        return ((fun_comp(fun_comp(anonymous_fun_23_,anonymous_fun_24_),anonymous_fun_29_))(par_map_input_1_),anonymous_fun_30_(par_map_input_1_))
      return ((fun_comp(fun_comp(anonymous_fun_15_,anonymous_fun_16_),anonymous_fun_21_))(par_map_input_2_),anonymous_fun_22_(par_map_input_2_))
    return ((fun_comp(fun_comp(anonymous_fun_7_,anonymous_fun_8_),anonymous_fun_13_))(par_map_input_3_),anonymous_fun_14_(par_map_input_3_))
  return anonymous_fun_6_
def anonymous_fun_45_(empty_closure_3_):
  """
  empty_closure_3_: ()
  """
  def anonymous_fun_46_(par_release_input_3_):
    """
    par_release_input_3_: (Double,(Double,(Double,(Double,Double))))
    """
    def anonymous_fun_47_(orange_input_1_):
      """
      orange_input_1_: Double
      """
      return laplace_fx(cfix(1.0),orange_input_1_)
    def anonymous_fun_48_(par_release_input_2_):
      """
      par_release_input_2_: (Double,(Double,(Double,Double)))
      """
      def anonymous_fun_49_(orange_input_2_):
        """
        orange_input_2_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_2_)
      def anonymous_fun_50_(par_release_input_1_):
        """
        par_release_input_1_: (Double,(Double,Double))
        """
        def anonymous_fun_51_(orange_input_3_):
          """
          orange_input_3_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_3_)
        def anonymous_fun_52_(par_release_input_0_):
          """
          par_release_input_0_: (Double,Double)
          """
          def anonymous_fun_53_(orange_input_4_):
            """
            orange_input_4_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_4_)
          def anonymous_fun_54_(orange_input_5_):
            """
            orange_input_5_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_5_)
          return (anonymous_fun_53_(par_release_input_0_[0]),anonymous_fun_54_(par_release_input_0_[1]))
        return (anonymous_fun_51_(par_release_input_1_[0]),anonymous_fun_52_(par_release_input_1_[1]))
      return (anonymous_fun_49_(par_release_input_2_[0]),anonymous_fun_50_(par_release_input_2_[1]))
    return (anonymous_fun_47_(par_release_input_3_[0]),anonymous_fun_48_(par_release_input_3_[1]))
  return anonymous_fun_46_
fused_vector_tup_0_ = bmcs(5,[1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_5_,(),anonymous_fun_45_)
fused_samples_0_ = [fused_vector_tup_0_[0],(fused_vector_tup_0_[1])[0],((fused_vector_tup_0_[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[1]]
def anonymous_fun_55_(curr_acc_4_):
  """
  curr_acc_4_: Vec Double
  """
  return len(curr_acc_4_) < len([0.0,0.0,0.0,0.0,0.0])
def anonymous_fun_56_(curr_acc_5_):
  """
  curr_acc_5_: Vec Double
  """
  def anonymous_fun_57_(curr_acc_6_):
    """
    curr_acc_6_: Vec Double
    """
    return len(curr_acc_6_) < len(fused_samples_0_)
  def anonymous_fun_58_(curr_acc_7_):
    """
    curr_acc_7_: Vec Double
    """
    return curr_acc_7_ + [(1.0e-3 / db_size_0_) * fused_samples_0_[len(curr_acc_7_)]]
  loop_acc_11_ = []
  while anonymous_fun_57_(loop_acc_11_):
    loop_acc_11_ = anonymous_fun_58_(loop_acc_11_)
  return curr_acc_5_ + [[0.0,0.0,0.0,0.0,0.0][len(curr_acc_5_)] + loop_acc_11_[len(curr_acc_5_)]]
loop_acc_10_ = []
while anonymous_fun_55_(loop_acc_10_):
  loop_acc_10_ = anonymous_fun_56_(loop_acc_10_)
loop_acc_10_