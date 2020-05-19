import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  def anonymous_fun_1_(row_10_):
    return row_10_[0]
  def anonymous_fun_2_(dbrow_0_):
    return dbrow_0_
  return fun_comp(anonymous_fun_1_,anonymous_fun_2_)
def anonymous_fun_3_(empty_closure_1_):
  def anonymous_fun_4_(orange_input_0_):
    return laplace_fx(cfix(1.0),orange_input_0_)
  return anonymous_fun_4_
x_sample_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
def anonymous_fun_5_(empty_closure_2_):
  def anonymous_fun_6_(row_9_):
    return row_9_[1]
  def anonymous_fun_7_(dbrow_1_):
    return dbrow_1_
  return fun_comp(anonymous_fun_6_,anonymous_fun_7_)
def anonymous_fun_8_(empty_closure_3_):
  def anonymous_fun_9_(orange_input_1_):
    return laplace_fx(cfix(1.0),orange_input_1_)
  return anonymous_fun_9_
x_sample_1_ = bmcs(1,[1.0],(),anonymous_fun_5_,(),anonymous_fun_8_)
def anonymous_fun_10_(empty_closure_4_):
  def anonymous_fun_11_(row_8_):
    return row_8_[2]
  def anonymous_fun_12_(dbrow_2_):
    return dbrow_2_
  return fun_comp(anonymous_fun_11_,anonymous_fun_12_)
def anonymous_fun_13_(empty_closure_5_):
  def anonymous_fun_14_(orange_input_2_):
    return laplace_fx(cfix(1.0),orange_input_2_)
  return anonymous_fun_14_
x_sample_2_ = bmcs(1,[1.0],(),anonymous_fun_10_,(),anonymous_fun_13_)
def anonymous_fun_15_(empty_closure_6_):
  def anonymous_fun_16_(row_7_):
    return row_7_[3]
  def anonymous_fun_17_(dbrow_3_):
    return dbrow_3_
  return fun_comp(anonymous_fun_16_,anonymous_fun_17_)
def anonymous_fun_18_(empty_closure_7_):
  def anonymous_fun_19_(orange_input_3_):
    return laplace_fx(cfix(1.0),orange_input_3_)
  return anonymous_fun_19_
x_sample_3_ = bmcs(1,[1.0],(),anonymous_fun_15_,(),anonymous_fun_18_)
def anonymous_fun_20_(empty_closure_8_):
  def anonymous_fun_21_(row_6_):
    return row_6_[4]
  def anonymous_fun_22_(dbrow_4_):
    return dbrow_4_
  return fun_comp(anonymous_fun_21_,anonymous_fun_22_)
def anonymous_fun_23_(empty_closure_9_):
  def anonymous_fun_24_(orange_input_4_):
    return laplace_fx(cfix(1.0),orange_input_4_)
  return anonymous_fun_24_
x_sample_4_ = bmcs(1,[1.0],(),anonymous_fun_20_,(),anonymous_fun_23_)
def anonymous_fun_25_(empty_closure_10_):
  def anonymous_fun_26_(row_5_):
    return row_5_[5]
  def anonymous_fun_27_(dbrow_5_):
    return dbrow_5_
  return fun_comp(anonymous_fun_26_,anonymous_fun_27_)
def anonymous_fun_28_(empty_closure_11_):
  def anonymous_fun_29_(orange_input_5_):
    return laplace_fx(cfix(1.0),orange_input_5_)
  return anonymous_fun_29_
x_sample_5_ = bmcs(1,[1.0],(),anonymous_fun_25_,(),anonymous_fun_28_)
def anonymous_fun_30_(empty_closure_12_):
  def anonymous_fun_31_(row_4_):
    return row_4_[6]
  def anonymous_fun_32_(dbrow_6_):
    return dbrow_6_
  return fun_comp(anonymous_fun_31_,anonymous_fun_32_)
def anonymous_fun_33_(empty_closure_13_):
  def anonymous_fun_34_(orange_input_6_):
    return laplace_fx(cfix(1.0),orange_input_6_)
  return anonymous_fun_34_
x_sample_6_ = bmcs(1,[1.0],(),anonymous_fun_30_,(),anonymous_fun_33_)
def anonymous_fun_35_(empty_closure_14_):
  def anonymous_fun_36_(row_3_):
    return row_3_[7]
  def anonymous_fun_37_(dbrow_7_):
    return dbrow_7_
  return fun_comp(anonymous_fun_36_,anonymous_fun_37_)
def anonymous_fun_38_(empty_closure_15_):
  def anonymous_fun_39_(orange_input_7_):
    return laplace_fx(cfix(1.0),orange_input_7_)
  return anonymous_fun_39_
x_sample_7_ = bmcs(1,[1.0],(),anonymous_fun_35_,(),anonymous_fun_38_)
def anonymous_fun_40_(empty_closure_16_):
  def anonymous_fun_41_(row_2_):
    return row_2_[8]
  def anonymous_fun_42_(dbrow_8_):
    return dbrow_8_
  return fun_comp(anonymous_fun_41_,anonymous_fun_42_)
def anonymous_fun_43_(empty_closure_17_):
  def anonymous_fun_44_(orange_input_8_):
    return laplace_fx(cfix(1.0),orange_input_8_)
  return anonymous_fun_44_
x_sample_8_ = bmcs(1,[1.0],(),anonymous_fun_40_,(),anonymous_fun_43_)
def anonymous_fun_45_(empty_closure_18_):
  def anonymous_fun_46_(row_1_):
    return row_1_[9]
  def anonymous_fun_47_(dbrow_9_):
    return dbrow_9_
  return fun_comp(anonymous_fun_46_,anonymous_fun_47_)
def anonymous_fun_48_(empty_closure_19_):
  def anonymous_fun_49_(orange_input_9_):
    return laplace_fx(cfix(1.0),orange_input_9_)
  return anonymous_fun_49_
x_sample_9_ = bmcs(1,[1.0],(),anonymous_fun_45_,(),anonymous_fun_48_)
def anonymous_fun_50_(empty_closure_20_):
  def anonymous_fun_51_(row_0_):
    return row_0_[10]
  def anonymous_fun_52_(dbrow_10_):
    return dbrow_10_
  return fun_comp(anonymous_fun_51_,anonymous_fun_52_)
def anonymous_fun_53_(empty_closure_21_):
  def anonymous_fun_54_(orange_input_10_):
    return laplace_fx(cfix(1.0),orange_input_10_)
  return anonymous_fun_54_
x_sample_10_ = bmcs(1,[1.0],(),anonymous_fun_50_,(),anonymous_fun_53_)
compressed_0_ = [x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + ([x_sample_5_] + ([x_sample_6_] + ([x_sample_7_] + ([x_sample_8_] + ([x_sample_9_] + ([x_sample_10_] + []))))))))))
def anonymous_fun_55_(curr_acc_0_):
  return len(curr_acc_0_) < len(compressed_0_)
def anonymous_fun_56_(curr_acc_1_):
  def anonymous_fun_57_(curr_acc_2_):
    return len(curr_acc_2_) < len(compressed_0_)
  def anonymous_fun_58_(curr_acc_3_):
    return curr_acc_3_ + [compressed_0_[len(curr_acc_1_)] * compressed_0_[len(curr_acc_3_)]]
  loop_acc_1_ = []
  while anonymous_fun_57_(loop_acc_1_):
    loop_acc_1_ = anonymous_fun_58_(loop_acc_1_)
  return curr_acc_1_ + [loop_acc_1_]
loop_acc_0_ = []
while anonymous_fun_55_(loop_acc_0_):
  loop_acc_0_ = anonymous_fun_56_(loop_acc_0_)
loop_acc_0_[0:3]