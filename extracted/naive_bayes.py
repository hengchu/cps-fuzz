import math
import mpc_math
def fun_comp(g, f):
  def inner(x):
    return g(f(x))
  return inner


def anonymous_fun_0_(empty_closure_0_):
  def anonymous_fun_1_(row_0_):
    if row_0_[9] == 1.0:
      cond_result_0_ = 1.0
    else:
      cond_result_0_ = 0.0
    return cond_result_0_
  def anonymous_fun_2_(dbrow_0_):
    return dbrow_0_
  return fun_comp(anonymous_fun_1_,anonymous_fun_2_)
def anonymous_fun_3_(empty_closure_1_):
  def anonymous_fun_4_(orange_input_0_):
    return laplace_fx(cfix(1.0),orange_input_0_)
  return anonymous_fun_4_
noised_label_sum_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
def anonymous_fun_5_(empty_closure_2_):
  def anonymous_fun_6_(par_map_input_0_):
    def anonymous_fun_7_(row_19_):
      if row_19_[9] == 1.0:
        cond_result_1_ = row_19_[0]
      else:
        cond_result_1_ = 0.0
      return cond_result_1_
    def anonymous_fun_8_(dbrow_1_):
      return dbrow_1_
    def anonymous_fun_9_(row_20_):
      if row_20_[9] == 0.0:
        cond_result_2_ = row_20_[0]
      else:
        cond_result_2_ = 0.0
      return cond_result_2_
    def anonymous_fun_10_(dbrow_2_):
      return dbrow_2_
    return ((fun_comp(anonymous_fun_7_,anonymous_fun_8_))(par_map_input_0_),(fun_comp(anonymous_fun_9_,anonymous_fun_10_))(par_map_input_0_))
  return anonymous_fun_6_
def anonymous_fun_11_(empty_closure_3_):
  def anonymous_fun_12_(par_release_input_0_):
    def anonymous_fun_13_(orange_input_1_):
      return laplace_fx(cfix(1.0),orange_input_1_)
    def anonymous_fun_14_(orange_input_2_):
      return laplace_fx(cfix(1.0),orange_input_2_)
    return (anonymous_fun_13_(par_release_input_0_[0]),anonymous_fun_14_(par_release_input_0_[1]))
  return anonymous_fun_12_
noised_sums_0_ = bmcs(2,[1.0,1.0],(),anonymous_fun_5_,(),anonymous_fun_11_)
x_sample_0_ = math.log((noised_sums_0_[0] / noised_label_sum_0_) / (noised_sums_0_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_0_[0] / noised_label_sum_0_) / (1.0 - noised_sums_0_[1] / noised_label_sum_0_))
def anonymous_fun_15_(empty_closure_4_):
  def anonymous_fun_16_(par_map_input_1_):
    def anonymous_fun_17_(row_17_):
      if row_17_[9] == 1.0:
        cond_result_3_ = row_17_[1]
      else:
        cond_result_3_ = 0.0
      return cond_result_3_
    def anonymous_fun_18_(dbrow_3_):
      return dbrow_3_
    def anonymous_fun_19_(row_18_):
      if row_18_[9] == 0.0:
        cond_result_4_ = row_18_[1]
      else:
        cond_result_4_ = 0.0
      return cond_result_4_
    def anonymous_fun_20_(dbrow_4_):
      return dbrow_4_
    return ((fun_comp(anonymous_fun_17_,anonymous_fun_18_))(par_map_input_1_),(fun_comp(anonymous_fun_19_,anonymous_fun_20_))(par_map_input_1_))
  return anonymous_fun_16_
def anonymous_fun_21_(empty_closure_5_):
  def anonymous_fun_22_(par_release_input_1_):
    def anonymous_fun_23_(orange_input_3_):
      return laplace_fx(cfix(1.0),orange_input_3_)
    def anonymous_fun_24_(orange_input_4_):
      return laplace_fx(cfix(1.0),orange_input_4_)
    return (anonymous_fun_23_(par_release_input_1_[0]),anonymous_fun_24_(par_release_input_1_[1]))
  return anonymous_fun_22_
noised_sums_1_ = bmcs(2,[1.0,1.0],(),anonymous_fun_15_,(),anonymous_fun_21_)
x_sample_1_ = math.log((noised_sums_1_[0] / noised_label_sum_0_) / (noised_sums_1_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_1_[0] / noised_label_sum_0_) / (1.0 - noised_sums_1_[1] / noised_label_sum_0_))
def anonymous_fun_25_(empty_closure_6_):
  def anonymous_fun_26_(par_map_input_2_):
    def anonymous_fun_27_(row_15_):
      if row_15_[9] == 1.0:
        cond_result_5_ = row_15_[2]
      else:
        cond_result_5_ = 0.0
      return cond_result_5_
    def anonymous_fun_28_(dbrow_5_):
      return dbrow_5_
    def anonymous_fun_29_(row_16_):
      if row_16_[9] == 0.0:
        cond_result_6_ = row_16_[2]
      else:
        cond_result_6_ = 0.0
      return cond_result_6_
    def anonymous_fun_30_(dbrow_6_):
      return dbrow_6_
    return ((fun_comp(anonymous_fun_27_,anonymous_fun_28_))(par_map_input_2_),(fun_comp(anonymous_fun_29_,anonymous_fun_30_))(par_map_input_2_))
  return anonymous_fun_26_
def anonymous_fun_31_(empty_closure_7_):
  def anonymous_fun_32_(par_release_input_2_):
    def anonymous_fun_33_(orange_input_5_):
      return laplace_fx(cfix(1.0),orange_input_5_)
    def anonymous_fun_34_(orange_input_6_):
      return laplace_fx(cfix(1.0),orange_input_6_)
    return (anonymous_fun_33_(par_release_input_2_[0]),anonymous_fun_34_(par_release_input_2_[1]))
  return anonymous_fun_32_
noised_sums_2_ = bmcs(2,[1.0,1.0],(),anonymous_fun_25_,(),anonymous_fun_31_)
x_sample_2_ = math.log((noised_sums_2_[0] / noised_label_sum_0_) / (noised_sums_2_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_2_[0] / noised_label_sum_0_) / (1.0 - noised_sums_2_[1] / noised_label_sum_0_))
def anonymous_fun_35_(empty_closure_8_):
  def anonymous_fun_36_(par_map_input_3_):
    def anonymous_fun_37_(row_13_):
      if row_13_[9] == 1.0:
        cond_result_7_ = row_13_[3]
      else:
        cond_result_7_ = 0.0
      return cond_result_7_
    def anonymous_fun_38_(dbrow_7_):
      return dbrow_7_
    def anonymous_fun_39_(row_14_):
      if row_14_[9] == 0.0:
        cond_result_8_ = row_14_[3]
      else:
        cond_result_8_ = 0.0
      return cond_result_8_
    def anonymous_fun_40_(dbrow_8_):
      return dbrow_8_
    return ((fun_comp(anonymous_fun_37_,anonymous_fun_38_))(par_map_input_3_),(fun_comp(anonymous_fun_39_,anonymous_fun_40_))(par_map_input_3_))
  return anonymous_fun_36_
def anonymous_fun_41_(empty_closure_9_):
  def anonymous_fun_42_(par_release_input_3_):
    def anonymous_fun_43_(orange_input_7_):
      return laplace_fx(cfix(1.0),orange_input_7_)
    def anonymous_fun_44_(orange_input_8_):
      return laplace_fx(cfix(1.0),orange_input_8_)
    return (anonymous_fun_43_(par_release_input_3_[0]),anonymous_fun_44_(par_release_input_3_[1]))
  return anonymous_fun_42_
noised_sums_3_ = bmcs(2,[1.0,1.0],(),anonymous_fun_35_,(),anonymous_fun_41_)
x_sample_3_ = math.log((noised_sums_3_[0] / noised_label_sum_0_) / (noised_sums_3_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_3_[0] / noised_label_sum_0_) / (1.0 - noised_sums_3_[1] / noised_label_sum_0_))
def anonymous_fun_45_(empty_closure_10_):
  def anonymous_fun_46_(par_map_input_4_):
    def anonymous_fun_47_(row_11_):
      if row_11_[9] == 1.0:
        cond_result_9_ = row_11_[4]
      else:
        cond_result_9_ = 0.0
      return cond_result_9_
    def anonymous_fun_48_(dbrow_9_):
      return dbrow_9_
    def anonymous_fun_49_(row_12_):
      if row_12_[9] == 0.0:
        cond_result_10_ = row_12_[4]
      else:
        cond_result_10_ = 0.0
      return cond_result_10_
    def anonymous_fun_50_(dbrow_10_):
      return dbrow_10_
    return ((fun_comp(anonymous_fun_47_,anonymous_fun_48_))(par_map_input_4_),(fun_comp(anonymous_fun_49_,anonymous_fun_50_))(par_map_input_4_))
  return anonymous_fun_46_
def anonymous_fun_51_(empty_closure_11_):
  def anonymous_fun_52_(par_release_input_4_):
    def anonymous_fun_53_(orange_input_9_):
      return laplace_fx(cfix(1.0),orange_input_9_)
    def anonymous_fun_54_(orange_input_10_):
      return laplace_fx(cfix(1.0),orange_input_10_)
    return (anonymous_fun_53_(par_release_input_4_[0]),anonymous_fun_54_(par_release_input_4_[1]))
  return anonymous_fun_52_
noised_sums_4_ = bmcs(2,[1.0,1.0],(),anonymous_fun_45_,(),anonymous_fun_51_)
x_sample_4_ = math.log((noised_sums_4_[0] / noised_label_sum_0_) / (noised_sums_4_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_4_[0] / noised_label_sum_0_) / (1.0 - noised_sums_4_[1] / noised_label_sum_0_))
def anonymous_fun_55_(empty_closure_12_):
  def anonymous_fun_56_(par_map_input_5_):
    def anonymous_fun_57_(row_9_):
      if row_9_[9] == 1.0:
        cond_result_11_ = row_9_[5]
      else:
        cond_result_11_ = 0.0
      return cond_result_11_
    def anonymous_fun_58_(dbrow_11_):
      return dbrow_11_
    def anonymous_fun_59_(row_10_):
      if row_10_[9] == 0.0:
        cond_result_12_ = row_10_[5]
      else:
        cond_result_12_ = 0.0
      return cond_result_12_
    def anonymous_fun_60_(dbrow_12_):
      return dbrow_12_
    return ((fun_comp(anonymous_fun_57_,anonymous_fun_58_))(par_map_input_5_),(fun_comp(anonymous_fun_59_,anonymous_fun_60_))(par_map_input_5_))
  return anonymous_fun_56_
def anonymous_fun_61_(empty_closure_13_):
  def anonymous_fun_62_(par_release_input_5_):
    def anonymous_fun_63_(orange_input_11_):
      return laplace_fx(cfix(1.0),orange_input_11_)
    def anonymous_fun_64_(orange_input_12_):
      return laplace_fx(cfix(1.0),orange_input_12_)
    return (anonymous_fun_63_(par_release_input_5_[0]),anonymous_fun_64_(par_release_input_5_[1]))
  return anonymous_fun_62_
noised_sums_5_ = bmcs(2,[1.0,1.0],(),anonymous_fun_55_,(),anonymous_fun_61_)
x_sample_5_ = math.log((noised_sums_5_[0] / noised_label_sum_0_) / (noised_sums_5_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_5_[0] / noised_label_sum_0_) / (1.0 - noised_sums_5_[1] / noised_label_sum_0_))
def anonymous_fun_65_(empty_closure_14_):
  def anonymous_fun_66_(par_map_input_6_):
    def anonymous_fun_67_(row_7_):
      if row_7_[9] == 1.0:
        cond_result_13_ = row_7_[6]
      else:
        cond_result_13_ = 0.0
      return cond_result_13_
    def anonymous_fun_68_(dbrow_13_):
      return dbrow_13_
    def anonymous_fun_69_(row_8_):
      if row_8_[9] == 0.0:
        cond_result_14_ = row_8_[6]
      else:
        cond_result_14_ = 0.0
      return cond_result_14_
    def anonymous_fun_70_(dbrow_14_):
      return dbrow_14_
    return ((fun_comp(anonymous_fun_67_,anonymous_fun_68_))(par_map_input_6_),(fun_comp(anonymous_fun_69_,anonymous_fun_70_))(par_map_input_6_))
  return anonymous_fun_66_
def anonymous_fun_71_(empty_closure_15_):
  def anonymous_fun_72_(par_release_input_6_):
    def anonymous_fun_73_(orange_input_13_):
      return laplace_fx(cfix(1.0),orange_input_13_)
    def anonymous_fun_74_(orange_input_14_):
      return laplace_fx(cfix(1.0),orange_input_14_)
    return (anonymous_fun_73_(par_release_input_6_[0]),anonymous_fun_74_(par_release_input_6_[1]))
  return anonymous_fun_72_
noised_sums_6_ = bmcs(2,[1.0,1.0],(),anonymous_fun_65_,(),anonymous_fun_71_)
x_sample_6_ = math.log((noised_sums_6_[0] / noised_label_sum_0_) / (noised_sums_6_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_6_[0] / noised_label_sum_0_) / (1.0 - noised_sums_6_[1] / noised_label_sum_0_))
def anonymous_fun_75_(empty_closure_16_):
  def anonymous_fun_76_(par_map_input_7_):
    def anonymous_fun_77_(row_5_):
      if row_5_[9] == 1.0:
        cond_result_15_ = row_5_[7]
      else:
        cond_result_15_ = 0.0
      return cond_result_15_
    def anonymous_fun_78_(dbrow_15_):
      return dbrow_15_
    def anonymous_fun_79_(row_6_):
      if row_6_[9] == 0.0:
        cond_result_16_ = row_6_[7]
      else:
        cond_result_16_ = 0.0
      return cond_result_16_
    def anonymous_fun_80_(dbrow_16_):
      return dbrow_16_
    return ((fun_comp(anonymous_fun_77_,anonymous_fun_78_))(par_map_input_7_),(fun_comp(anonymous_fun_79_,anonymous_fun_80_))(par_map_input_7_))
  return anonymous_fun_76_
def anonymous_fun_81_(empty_closure_17_):
  def anonymous_fun_82_(par_release_input_7_):
    def anonymous_fun_83_(orange_input_15_):
      return laplace_fx(cfix(1.0),orange_input_15_)
    def anonymous_fun_84_(orange_input_16_):
      return laplace_fx(cfix(1.0),orange_input_16_)
    return (anonymous_fun_83_(par_release_input_7_[0]),anonymous_fun_84_(par_release_input_7_[1]))
  return anonymous_fun_82_
noised_sums_7_ = bmcs(2,[1.0,1.0],(),anonymous_fun_75_,(),anonymous_fun_81_)
x_sample_7_ = math.log((noised_sums_7_[0] / noised_label_sum_0_) / (noised_sums_7_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_7_[0] / noised_label_sum_0_) / (1.0 - noised_sums_7_[1] / noised_label_sum_0_))
def anonymous_fun_85_(empty_closure_18_):
  def anonymous_fun_86_(par_map_input_8_):
    def anonymous_fun_87_(row_3_):
      if row_3_[9] == 1.0:
        cond_result_17_ = row_3_[8]
      else:
        cond_result_17_ = 0.0
      return cond_result_17_
    def anonymous_fun_88_(dbrow_17_):
      return dbrow_17_
    def anonymous_fun_89_(row_4_):
      if row_4_[9] == 0.0:
        cond_result_18_ = row_4_[8]
      else:
        cond_result_18_ = 0.0
      return cond_result_18_
    def anonymous_fun_90_(dbrow_18_):
      return dbrow_18_
    return ((fun_comp(anonymous_fun_87_,anonymous_fun_88_))(par_map_input_8_),(fun_comp(anonymous_fun_89_,anonymous_fun_90_))(par_map_input_8_))
  return anonymous_fun_86_
def anonymous_fun_91_(empty_closure_19_):
  def anonymous_fun_92_(par_release_input_8_):
    def anonymous_fun_93_(orange_input_17_):
      return laplace_fx(cfix(1.0),orange_input_17_)
    def anonymous_fun_94_(orange_input_18_):
      return laplace_fx(cfix(1.0),orange_input_18_)
    return (anonymous_fun_93_(par_release_input_8_[0]),anonymous_fun_94_(par_release_input_8_[1]))
  return anonymous_fun_92_
noised_sums_8_ = bmcs(2,[1.0,1.0],(),anonymous_fun_85_,(),anonymous_fun_91_)
x_sample_8_ = math.log((noised_sums_8_[0] / noised_label_sum_0_) / (noised_sums_8_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_8_[0] / noised_label_sum_0_) / (1.0 - noised_sums_8_[1] / noised_label_sum_0_))
def anonymous_fun_95_(empty_closure_20_):
  def anonymous_fun_96_(par_map_input_9_):
    def anonymous_fun_97_(row_1_):
      if row_1_[9] == 1.0:
        cond_result_19_ = row_1_[9]
      else:
        cond_result_19_ = 0.0
      return cond_result_19_
    def anonymous_fun_98_(dbrow_19_):
      return dbrow_19_
    def anonymous_fun_99_(row_2_):
      if row_2_[9] == 0.0:
        cond_result_20_ = row_2_[9]
      else:
        cond_result_20_ = 0.0
      return cond_result_20_
    def anonymous_fun_100_(dbrow_20_):
      return dbrow_20_
    return ((fun_comp(anonymous_fun_97_,anonymous_fun_98_))(par_map_input_9_),(fun_comp(anonymous_fun_99_,anonymous_fun_100_))(par_map_input_9_))
  return anonymous_fun_96_
def anonymous_fun_101_(empty_closure_21_):
  def anonymous_fun_102_(par_release_input_9_):
    def anonymous_fun_103_(orange_input_19_):
      return laplace_fx(cfix(1.0),orange_input_19_)
    def anonymous_fun_104_(orange_input_20_):
      return laplace_fx(cfix(1.0),orange_input_20_)
    return (anonymous_fun_103_(par_release_input_9_[0]),anonymous_fun_104_(par_release_input_9_[1]))
  return anonymous_fun_102_
noised_sums_9_ = bmcs(2,[1.0,1.0],(),anonymous_fun_95_,(),anonymous_fun_101_)
x_sample_9_ = math.log((noised_sums_9_[0] / noised_label_sum_0_) / (noised_sums_9_[1] / noised_label_sum_0_)) - math.log((1.0 - noised_sums_9_[0] / noised_label_sum_0_) / (1.0 - noised_sums_9_[1] / noised_label_sum_0_))
[x_sample_0_] + ([x_sample_1_] + ([x_sample_2_] + ([x_sample_3_] + ([x_sample_4_] + ([x_sample_5_] + ([x_sample_6_] + ([x_sample_7_] + ([x_sample_8_] + ([x_sample_9_] + [])))))))))