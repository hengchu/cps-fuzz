import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  def anonymous_fun_1_(row_0_):
    def anonymous_fun_2_(curr_acc_0_):
      return curr_acc_0_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_3_(curr_acc_1_):
      return (curr_acc_1_[0] + 1,curr_acc_1_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_1_[0]] * (row_0_[0:5])[curr_acc_1_[0]])
    loop_acc_0_ = (0,0.0)
    while anonymous_fun_2_(loop_acc_0_):
      loop_acc_0_ = anonymous_fun_3_(loop_acc_0_)
    if row_0_[5] * loop_acc_0_[1] < 0.0:
      cond_result_0_ = 1.0
    else:
      cond_result_0_ = 0.0
    return cond_result_0_
  def anonymous_fun_4_(dbrow_0_):
    return dbrow_0_
  return fun_comp(anonymous_fun_1_,anonymous_fun_4_)
def anonymous_fun_5_(empty_closure_1_):
  def anonymous_fun_6_(orange_input_0_):
    return laplace_fx(cfix(1.0),orange_input_0_)
  return anonymous_fun_6_
noised_incorrect_count_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_5_)
def anonymous_fun_7_(empty_closure_2_):
  def anonymous_fun_8_(row_5_):
    def anonymous_fun_9_(curr_acc_10_):
      return curr_acc_10_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_10_(curr_acc_11_):
      return (curr_acc_11_[0] + 1,curr_acc_11_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_11_[0]] * (row_5_[0:5])[curr_acc_11_[0]])
    loop_acc_1_ = (0,0.0)
    while anonymous_fun_9_(loop_acc_1_):
      loop_acc_1_ = anonymous_fun_10_(loop_acc_1_)
    if row_5_[5] * loop_acc_1_[1] < 0.0:
      cond_result_1_ = row_5_[0]
    else:
      cond_result_1_ = 0.0
    return cond_result_1_
  def anonymous_fun_11_(dbrow_1_):
    return dbrow_1_
  return fun_comp(anonymous_fun_8_,anonymous_fun_11_)
def anonymous_fun_12_(empty_closure_3_):
  def anonymous_fun_13_(orange_input_1_):
    return laplace_fx(cfix(1.0),orange_input_1_)
  return anonymous_fun_13_
x_sample_0_ = bmcs(1,[1.0],(),anonymous_fun_7_,(),anonymous_fun_12_)
def anonymous_fun_14_(empty_closure_4_):
  def anonymous_fun_15_(row_4_):
    def anonymous_fun_16_(curr_acc_8_):
      return curr_acc_8_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_17_(curr_acc_9_):
      return (curr_acc_9_[0] + 1,curr_acc_9_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_9_[0]] * (row_4_[0:5])[curr_acc_9_[0]])
    loop_acc_2_ = (0,0.0)
    while anonymous_fun_16_(loop_acc_2_):
      loop_acc_2_ = anonymous_fun_17_(loop_acc_2_)
    if row_4_[5] * loop_acc_2_[1] < 0.0:
      cond_result_2_ = row_4_[1]
    else:
      cond_result_2_ = 0.0
    return cond_result_2_
  def anonymous_fun_18_(dbrow_2_):
    return dbrow_2_
  return fun_comp(anonymous_fun_15_,anonymous_fun_18_)
def anonymous_fun_19_(empty_closure_5_):
  def anonymous_fun_20_(orange_input_2_):
    return laplace_fx(cfix(1.0),orange_input_2_)
  return anonymous_fun_20_
x_sample_1_ = bmcs(1,[1.0],(),anonymous_fun_14_,(),anonymous_fun_19_)
def anonymous_fun_21_(empty_closure_6_):
  def anonymous_fun_22_(row_3_):
    def anonymous_fun_23_(curr_acc_6_):
      return curr_acc_6_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_24_(curr_acc_7_):
      return (curr_acc_7_[0] + 1,curr_acc_7_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_7_[0]] * (row_3_[0:5])[curr_acc_7_[0]])
    loop_acc_3_ = (0,0.0)
    while anonymous_fun_23_(loop_acc_3_):
      loop_acc_3_ = anonymous_fun_24_(loop_acc_3_)
    if row_3_[5] * loop_acc_3_[1] < 0.0:
      cond_result_3_ = row_3_[2]
    else:
      cond_result_3_ = 0.0
    return cond_result_3_
  def anonymous_fun_25_(dbrow_3_):
    return dbrow_3_
  return fun_comp(anonymous_fun_22_,anonymous_fun_25_)
def anonymous_fun_26_(empty_closure_7_):
  def anonymous_fun_27_(orange_input_3_):
    return laplace_fx(cfix(1.0),orange_input_3_)
  return anonymous_fun_27_
x_sample_2_ = bmcs(1,[1.0],(),anonymous_fun_21_,(),anonymous_fun_26_)
def anonymous_fun_28_(empty_closure_8_):
  def anonymous_fun_29_(row_2_):
    def anonymous_fun_30_(curr_acc_4_):
      return curr_acc_4_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_31_(curr_acc_5_):
      return (curr_acc_5_[0] + 1,curr_acc_5_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_5_[0]] * (row_2_[0:5])[curr_acc_5_[0]])
    loop_acc_4_ = (0,0.0)
    while anonymous_fun_30_(loop_acc_4_):
      loop_acc_4_ = anonymous_fun_31_(loop_acc_4_)
    if row_2_[5] * loop_acc_4_[1] < 0.0:
      cond_result_4_ = row_2_[3]
    else:
      cond_result_4_ = 0.0
    return cond_result_4_
  def anonymous_fun_32_(dbrow_4_):
    return dbrow_4_
  return fun_comp(anonymous_fun_29_,anonymous_fun_32_)
def anonymous_fun_33_(empty_closure_9_):
  def anonymous_fun_34_(orange_input_4_):
    return laplace_fx(cfix(1.0),orange_input_4_)
  return anonymous_fun_34_
x_sample_3_ = bmcs(1,[1.0],(),anonymous_fun_28_,(),anonymous_fun_33_)
def anonymous_fun_35_(empty_closure_10_):
  def anonymous_fun_36_(row_1_):
    def anonymous_fun_37_(curr_acc_2_):
      return curr_acc_2_[0] < len([0.0,0.0,0.0,0.0,0.0])
    def anonymous_fun_38_(curr_acc_3_):
      return (curr_acc_3_[0] + 1,curr_acc_3_[1] + [0.0,0.0,0.0,0.0,0.0][curr_acc_3_[0]] * (row_1_[0:5])[curr_acc_3_[0]])
    loop_acc_5_ = (0,0.0)
    while anonymous_fun_37_(loop_acc_5_):
      loop_acc_5_ = anonymous_fun_38_(loop_acc_5_)
    if row_1_[5] * loop_acc_5_[1] < 0.0:
      cond_result_5_ = row_1_[4]
    else:
      cond_result_5_ = 0.0
    return cond_result_5_
  def anonymous_fun_39_(dbrow_5_):
    return dbrow_5_
  return fun_comp(anonymous_fun_36_,anonymous_fun_39_)
def anonymous_fun_40_(empty_closure_11_):
  def anonymous_fun_41_(orange_input_5_):
    return laplace_fx(cfix(1.0),orange_input_5_)
  return anonymous_fun_41_
x_sample_4_ = bmcs(1,[1.0],(),anonymous_fun_35_,(),anonymous_fun_40_)
def anonymous_fun_42_(curr_acc_12_):
  return len(curr_acc_12_) < len([0.0,0.0,0.0,0.0,0.0])
def anonymous_fun_43_(curr_acc_13_):
  def anonymous_fun_44_(curr_acc_14_):
    return len(curr_acc_14_) < len([x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + [])))))
  def anonymous_fun_45_(curr_acc_15_):
    return curr_acc_15_ + [(1.0 / noised_incorrect_count_0_) * ([x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + [])))))[len(curr_acc_15_)]]
  loop_acc_7_ = []
  while anonymous_fun_44_(loop_acc_7_):
    loop_acc_7_ = anonymous_fun_45_(loop_acc_7_)
  return curr_acc_13_ + [[0.0,0.0,0.0,0.0,0.0][len(curr_acc_13_)] + loop_acc_7_[len(curr_acc_13_)]]
loop_acc_6_ = []
while anonymous_fun_42_(loop_acc_6_):
  loop_acc_6_ = anonymous_fun_43_(loop_acc_6_)
loop_acc_6_