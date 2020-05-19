import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  def anonymous_fun_1_(row_0_):
    return 1.0
  def anonymous_fun_2_(dbrow_0_):
    return dbrow_0_
  return fun_comp(anonymous_fun_1_,anonymous_fun_2_)
def anonymous_fun_3_(empty_closure_1_):
  def anonymous_fun_4_(orange_input_0_):
    return laplace_fx(cfix(1.0),orange_input_0_)
  return anonymous_fun_4_
db_size_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
def anonymous_fun_5_(empty_closure_2_):
  def anonymous_fun_6_(g_4_):
    return g_4_[0]
  def anonymous_fun_7_(row_1_):
    def anonymous_fun_8_(curr_acc_0_):
      return len(curr_acc_0_) < len(row_1_)
    def anonymous_fun_9_(curr_acc_1_):
      def anonymous_fun_10_(curr_acc_2_):
        return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
      def anonymous_fun_11_(curr_acc_3_):
        return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
      loop_acc_1_ = (0,0.0)
      while anonymous_fun_10_(loop_acc_1_):
        loop_acc_1_ = anonymous_fun_11_(loop_acc_1_)
      return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_1_[1])))) * row_1_[len(curr_acc_1_)]]
    loop_acc_0_ = []
    while anonymous_fun_8_(loop_acc_0_):
      loop_acc_0_ = anonymous_fun_9_(loop_acc_0_)
    return loop_acc_0_
  def anonymous_fun_12_(dbrow_1_):
    return dbrow_1_
  return fun_comp(fun_comp(anonymous_fun_6_,anonymous_fun_7_),anonymous_fun_12_)
def anonymous_fun_13_(empty_closure_3_):
  def anonymous_fun_14_(orange_input_1_):
    return laplace_fx(cfix(1.0),orange_input_1_)
  return anonymous_fun_14_
x_sample_0_ = bmcs(1,[1.0],(),anonymous_fun_5_,(),anonymous_fun_13_)
def anonymous_fun_15_(empty_closure_4_):
  def anonymous_fun_16_(g_3_):
    return g_3_[1]
  def anonymous_fun_17_(row_1_):
    def anonymous_fun_18_(curr_acc_0_):
      return len(curr_acc_0_) < len(row_1_)
    def anonymous_fun_19_(curr_acc_1_):
      def anonymous_fun_20_(curr_acc_2_):
        return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
      def anonymous_fun_21_(curr_acc_3_):
        return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
      loop_acc_3_ = (0,0.0)
      while anonymous_fun_20_(loop_acc_3_):
        loop_acc_3_ = anonymous_fun_21_(loop_acc_3_)
      return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_3_[1])))) * row_1_[len(curr_acc_1_)]]
    loop_acc_2_ = []
    while anonymous_fun_18_(loop_acc_2_):
      loop_acc_2_ = anonymous_fun_19_(loop_acc_2_)
    return loop_acc_2_
  def anonymous_fun_22_(dbrow_2_):
    return dbrow_2_
  return fun_comp(fun_comp(anonymous_fun_16_,anonymous_fun_17_),anonymous_fun_22_)
def anonymous_fun_23_(empty_closure_5_):
  def anonymous_fun_24_(orange_input_2_):
    return laplace_fx(cfix(1.0),orange_input_2_)
  return anonymous_fun_24_
x_sample_1_ = bmcs(1,[1.0],(),anonymous_fun_15_,(),anonymous_fun_23_)
def anonymous_fun_25_(empty_closure_6_):
  def anonymous_fun_26_(g_2_):
    return g_2_[2]
  def anonymous_fun_27_(row_1_):
    def anonymous_fun_28_(curr_acc_0_):
      return len(curr_acc_0_) < len(row_1_)
    def anonymous_fun_29_(curr_acc_1_):
      def anonymous_fun_30_(curr_acc_2_):
        return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
      def anonymous_fun_31_(curr_acc_3_):
        return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
      loop_acc_5_ = (0,0.0)
      while anonymous_fun_30_(loop_acc_5_):
        loop_acc_5_ = anonymous_fun_31_(loop_acc_5_)
      return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_5_[1])))) * row_1_[len(curr_acc_1_)]]
    loop_acc_4_ = []
    while anonymous_fun_28_(loop_acc_4_):
      loop_acc_4_ = anonymous_fun_29_(loop_acc_4_)
    return loop_acc_4_
  def anonymous_fun_32_(dbrow_3_):
    return dbrow_3_
  return fun_comp(fun_comp(anonymous_fun_26_,anonymous_fun_27_),anonymous_fun_32_)
def anonymous_fun_33_(empty_closure_7_):
  def anonymous_fun_34_(orange_input_3_):
    return laplace_fx(cfix(1.0),orange_input_3_)
  return anonymous_fun_34_
x_sample_2_ = bmcs(1,[1.0],(),anonymous_fun_25_,(),anonymous_fun_33_)
def anonymous_fun_35_(empty_closure_8_):
  def anonymous_fun_36_(g_1_):
    return g_1_[3]
  def anonymous_fun_37_(row_1_):
    def anonymous_fun_38_(curr_acc_0_):
      return len(curr_acc_0_) < len(row_1_)
    def anonymous_fun_39_(curr_acc_1_):
      def anonymous_fun_40_(curr_acc_2_):
        return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
      def anonymous_fun_41_(curr_acc_3_):
        return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
      loop_acc_7_ = (0,0.0)
      while anonymous_fun_40_(loop_acc_7_):
        loop_acc_7_ = anonymous_fun_41_(loop_acc_7_)
      return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_7_[1])))) * row_1_[len(curr_acc_1_)]]
    loop_acc_6_ = []
    while anonymous_fun_38_(loop_acc_6_):
      loop_acc_6_ = anonymous_fun_39_(loop_acc_6_)
    return loop_acc_6_
  def anonymous_fun_42_(dbrow_4_):
    return dbrow_4_
  return fun_comp(fun_comp(anonymous_fun_36_,anonymous_fun_37_),anonymous_fun_42_)
def anonymous_fun_43_(empty_closure_9_):
  def anonymous_fun_44_(orange_input_4_):
    return laplace_fx(cfix(1.0),orange_input_4_)
  return anonymous_fun_44_
x_sample_3_ = bmcs(1,[1.0],(),anonymous_fun_35_,(),anonymous_fun_43_)
def anonymous_fun_45_(empty_closure_10_):
  def anonymous_fun_46_(g_0_):
    return g_0_[4]
  def anonymous_fun_47_(row_1_):
    def anonymous_fun_48_(curr_acc_0_):
      return len(curr_acc_0_) < len(row_1_)
    def anonymous_fun_49_(curr_acc_1_):
      def anonymous_fun_50_(curr_acc_2_):
        return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
      def anonymous_fun_51_(curr_acc_3_):
        return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * row_1_[curr_acc_3_[0]])
      loop_acc_9_ = (0,0.0)
      while anonymous_fun_50_(loop_acc_9_):
        loop_acc_9_ = anonymous_fun_51_(loop_acc_9_)
      return curr_acc_1_ + [(row_1_[len(row_1_) - 1] * (1.0 - 1.0 / (1.0 + math.exp(0.0 - (1.0 * row_1_[len(row_1_) - 1]) * loop_acc_9_[1])))) * row_1_[len(curr_acc_1_)]]
    loop_acc_8_ = []
    while anonymous_fun_48_(loop_acc_8_):
      loop_acc_8_ = anonymous_fun_49_(loop_acc_8_)
    return loop_acc_8_
  def anonymous_fun_52_(dbrow_5_):
    return dbrow_5_
  return fun_comp(fun_comp(anonymous_fun_46_,anonymous_fun_47_),anonymous_fun_52_)
def anonymous_fun_53_(empty_closure_11_):
  def anonymous_fun_54_(orange_input_5_):
    return laplace_fx(cfix(1.0),orange_input_5_)
  return anonymous_fun_54_
x_sample_4_ = bmcs(1,[1.0],(),anonymous_fun_45_,(),anonymous_fun_53_)
def anonymous_fun_55_(curr_acc_4_):
  return len(curr_acc_4_) < len([0.0,0.0,0.0,0.0,0.0])
def anonymous_fun_56_(curr_acc_5_):
  def anonymous_fun_57_(curr_acc_6_):
    return len(curr_acc_6_) < len([x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + [])))))
  def anonymous_fun_58_(curr_acc_7_):
    return curr_acc_7_ + [(1.0e-3 / db_size_0_) * ([x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + [])))))[len(curr_acc_7_)]]
  loop_acc_11_ = []
  while anonymous_fun_57_(loop_acc_11_):
    loop_acc_11_ = anonymous_fun_58_(loop_acc_11_)
  return curr_acc_5_ + [[0.0,0.0,0.0,0.0,0.0][len(curr_acc_5_)] + loop_acc_11_[len(curr_acc_5_)]]
loop_acc_10_ = []
while anonymous_fun_55_(loop_acc_10_):
  loop_acc_10_ = anonymous_fun_56_(loop_acc_10_)
loop_acc_10_