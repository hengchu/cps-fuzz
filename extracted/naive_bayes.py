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
    if row_0_[9] == 1.0:
      cond_result_0_ = 1.0
    else:
      cond_result_0_ = 0.0
    return cond_result_0_
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
noised_label_sum_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
def anonymous_fun_5_(empty_closure_2_):
  """
  empty_closure_2_: ()
  """
  def anonymous_fun_6_(par_map_input_18_):
    """
    par_map_input_18_: Vec Double
    """
    def anonymous_fun_7_(par_map_input_0_):
      """
      par_map_input_0_: Vec Double
      """
      def anonymous_fun_8_(row_19_):
        """
        row_19_: Vec Double
        """
        if row_19_[9] == 1.0:
          cond_result_1_ = row_19_[0]
        else:
          cond_result_1_ = 0.0
        return cond_result_1_
      def anonymous_fun_9_(dbrow_1_):
        """
        dbrow_1_: Vec Double
        """
        return dbrow_1_
      def anonymous_fun_10_(row_20_):
        """
        row_20_: Vec Double
        """
        if row_20_[9] == 0.0:
          cond_result_2_ = row_20_[0]
        else:
          cond_result_2_ = 0.0
        return cond_result_2_
      def anonymous_fun_11_(dbrow_2_):
        """
        dbrow_2_: Vec Double
        """
        return dbrow_2_
      return ((fun_comp(anonymous_fun_8_,anonymous_fun_9_))(par_map_input_0_),(fun_comp(anonymous_fun_10_,anonymous_fun_11_))(par_map_input_0_))
    def anonymous_fun_12_(par_map_input_17_):
      """
      par_map_input_17_: Vec Double
      """
      def anonymous_fun_13_(par_map_input_1_):
        """
        par_map_input_1_: Vec Double
        """
        def anonymous_fun_14_(row_17_):
          """
          row_17_: Vec Double
          """
          if row_17_[9] == 1.0:
            cond_result_3_ = row_17_[1]
          else:
            cond_result_3_ = 0.0
          return cond_result_3_
        def anonymous_fun_15_(dbrow_3_):
          """
          dbrow_3_: Vec Double
          """
          return dbrow_3_
        def anonymous_fun_16_(row_18_):
          """
          row_18_: Vec Double
          """
          if row_18_[9] == 0.0:
            cond_result_4_ = row_18_[1]
          else:
            cond_result_4_ = 0.0
          return cond_result_4_
        def anonymous_fun_17_(dbrow_4_):
          """
          dbrow_4_: Vec Double
          """
          return dbrow_4_
        return ((fun_comp(anonymous_fun_14_,anonymous_fun_15_))(par_map_input_1_),(fun_comp(anonymous_fun_16_,anonymous_fun_17_))(par_map_input_1_))
      def anonymous_fun_18_(par_map_input_16_):
        """
        par_map_input_16_: Vec Double
        """
        def anonymous_fun_19_(par_map_input_2_):
          """
          par_map_input_2_: Vec Double
          """
          def anonymous_fun_20_(row_15_):
            """
            row_15_: Vec Double
            """
            if row_15_[9] == 1.0:
              cond_result_5_ = row_15_[2]
            else:
              cond_result_5_ = 0.0
            return cond_result_5_
          def anonymous_fun_21_(dbrow_5_):
            """
            dbrow_5_: Vec Double
            """
            return dbrow_5_
          def anonymous_fun_22_(row_16_):
            """
            row_16_: Vec Double
            """
            if row_16_[9] == 0.0:
              cond_result_6_ = row_16_[2]
            else:
              cond_result_6_ = 0.0
            return cond_result_6_
          def anonymous_fun_23_(dbrow_6_):
            """
            dbrow_6_: Vec Double
            """
            return dbrow_6_
          return ((fun_comp(anonymous_fun_20_,anonymous_fun_21_))(par_map_input_2_),(fun_comp(anonymous_fun_22_,anonymous_fun_23_))(par_map_input_2_))
        def anonymous_fun_24_(par_map_input_15_):
          """
          par_map_input_15_: Vec Double
          """
          def anonymous_fun_25_(par_map_input_3_):
            """
            par_map_input_3_: Vec Double
            """
            def anonymous_fun_26_(row_13_):
              """
              row_13_: Vec Double
              """
              if row_13_[9] == 1.0:
                cond_result_7_ = row_13_[3]
              else:
                cond_result_7_ = 0.0
              return cond_result_7_
            def anonymous_fun_27_(dbrow_7_):
              """
              dbrow_7_: Vec Double
              """
              return dbrow_7_
            def anonymous_fun_28_(row_14_):
              """
              row_14_: Vec Double
              """
              if row_14_[9] == 0.0:
                cond_result_8_ = row_14_[3]
              else:
                cond_result_8_ = 0.0
              return cond_result_8_
            def anonymous_fun_29_(dbrow_8_):
              """
              dbrow_8_: Vec Double
              """
              return dbrow_8_
            return ((fun_comp(anonymous_fun_26_,anonymous_fun_27_))(par_map_input_3_),(fun_comp(anonymous_fun_28_,anonymous_fun_29_))(par_map_input_3_))
          def anonymous_fun_30_(par_map_input_14_):
            """
            par_map_input_14_: Vec Double
            """
            def anonymous_fun_31_(par_map_input_4_):
              """
              par_map_input_4_: Vec Double
              """
              def anonymous_fun_32_(row_11_):
                """
                row_11_: Vec Double
                """
                if row_11_[9] == 1.0:
                  cond_result_9_ = row_11_[4]
                else:
                  cond_result_9_ = 0.0
                return cond_result_9_
              def anonymous_fun_33_(dbrow_9_):
                """
                dbrow_9_: Vec Double
                """
                return dbrow_9_
              def anonymous_fun_34_(row_12_):
                """
                row_12_: Vec Double
                """
                if row_12_[9] == 0.0:
                  cond_result_10_ = row_12_[4]
                else:
                  cond_result_10_ = 0.0
                return cond_result_10_
              def anonymous_fun_35_(dbrow_10_):
                """
                dbrow_10_: Vec Double
                """
                return dbrow_10_
              return ((fun_comp(anonymous_fun_32_,anonymous_fun_33_))(par_map_input_4_),(fun_comp(anonymous_fun_34_,anonymous_fun_35_))(par_map_input_4_))
            def anonymous_fun_36_(par_map_input_13_):
              """
              par_map_input_13_: Vec Double
              """
              def anonymous_fun_37_(par_map_input_5_):
                """
                par_map_input_5_: Vec Double
                """
                def anonymous_fun_38_(row_9_):
                  """
                  row_9_: Vec Double
                  """
                  if row_9_[9] == 1.0:
                    cond_result_11_ = row_9_[5]
                  else:
                    cond_result_11_ = 0.0
                  return cond_result_11_
                def anonymous_fun_39_(dbrow_11_):
                  """
                  dbrow_11_: Vec Double
                  """
                  return dbrow_11_
                def anonymous_fun_40_(row_10_):
                  """
                  row_10_: Vec Double
                  """
                  if row_10_[9] == 0.0:
                    cond_result_12_ = row_10_[5]
                  else:
                    cond_result_12_ = 0.0
                  return cond_result_12_
                def anonymous_fun_41_(dbrow_12_):
                  """
                  dbrow_12_: Vec Double
                  """
                  return dbrow_12_
                return ((fun_comp(anonymous_fun_38_,anonymous_fun_39_))(par_map_input_5_),(fun_comp(anonymous_fun_40_,anonymous_fun_41_))(par_map_input_5_))
              def anonymous_fun_42_(par_map_input_12_):
                """
                par_map_input_12_: Vec Double
                """
                def anonymous_fun_43_(par_map_input_6_):
                  """
                  par_map_input_6_: Vec Double
                  """
                  def anonymous_fun_44_(row_7_):
                    """
                    row_7_: Vec Double
                    """
                    if row_7_[9] == 1.0:
                      cond_result_13_ = row_7_[6]
                    else:
                      cond_result_13_ = 0.0
                    return cond_result_13_
                  def anonymous_fun_45_(dbrow_13_):
                    """
                    dbrow_13_: Vec Double
                    """
                    return dbrow_13_
                  def anonymous_fun_46_(row_8_):
                    """
                    row_8_: Vec Double
                    """
                    if row_8_[9] == 0.0:
                      cond_result_14_ = row_8_[6]
                    else:
                      cond_result_14_ = 0.0
                    return cond_result_14_
                  def anonymous_fun_47_(dbrow_14_):
                    """
                    dbrow_14_: Vec Double
                    """
                    return dbrow_14_
                  return ((fun_comp(anonymous_fun_44_,anonymous_fun_45_))(par_map_input_6_),(fun_comp(anonymous_fun_46_,anonymous_fun_47_))(par_map_input_6_))
                def anonymous_fun_48_(par_map_input_11_):
                  """
                  par_map_input_11_: Vec Double
                  """
                  def anonymous_fun_49_(par_map_input_7_):
                    """
                    par_map_input_7_: Vec Double
                    """
                    def anonymous_fun_50_(row_5_):
                      """
                      row_5_: Vec Double
                      """
                      if row_5_[9] == 1.0:
                        cond_result_15_ = row_5_[7]
                      else:
                        cond_result_15_ = 0.0
                      return cond_result_15_
                    def anonymous_fun_51_(dbrow_15_):
                      """
                      dbrow_15_: Vec Double
                      """
                      return dbrow_15_
                    def anonymous_fun_52_(row_6_):
                      """
                      row_6_: Vec Double
                      """
                      if row_6_[9] == 0.0:
                        cond_result_16_ = row_6_[7]
                      else:
                        cond_result_16_ = 0.0
                      return cond_result_16_
                    def anonymous_fun_53_(dbrow_16_):
                      """
                      dbrow_16_: Vec Double
                      """
                      return dbrow_16_
                    return ((fun_comp(anonymous_fun_50_,anonymous_fun_51_))(par_map_input_7_),(fun_comp(anonymous_fun_52_,anonymous_fun_53_))(par_map_input_7_))
                  def anonymous_fun_54_(par_map_input_10_):
                    """
                    par_map_input_10_: Vec Double
                    """
                    def anonymous_fun_55_(par_map_input_8_):
                      """
                      par_map_input_8_: Vec Double
                      """
                      def anonymous_fun_56_(row_3_):
                        """
                        row_3_: Vec Double
                        """
                        if row_3_[9] == 1.0:
                          cond_result_17_ = row_3_[8]
                        else:
                          cond_result_17_ = 0.0
                        return cond_result_17_
                      def anonymous_fun_57_(dbrow_17_):
                        """
                        dbrow_17_: Vec Double
                        """
                        return dbrow_17_
                      def anonymous_fun_58_(row_4_):
                        """
                        row_4_: Vec Double
                        """
                        if row_4_[9] == 0.0:
                          cond_result_18_ = row_4_[8]
                        else:
                          cond_result_18_ = 0.0
                        return cond_result_18_
                      def anonymous_fun_59_(dbrow_18_):
                        """
                        dbrow_18_: Vec Double
                        """
                        return dbrow_18_
                      return ((fun_comp(anonymous_fun_56_,anonymous_fun_57_))(par_map_input_8_),(fun_comp(anonymous_fun_58_,anonymous_fun_59_))(par_map_input_8_))
                    def anonymous_fun_60_(par_map_input_9_):
                      """
                      par_map_input_9_: Vec Double
                      """
                      def anonymous_fun_61_(row_1_):
                        """
                        row_1_: Vec Double
                        """
                        if row_1_[9] == 1.0:
                          cond_result_19_ = row_1_[9]
                        else:
                          cond_result_19_ = 0.0
                        return cond_result_19_
                      def anonymous_fun_62_(dbrow_19_):
                        """
                        dbrow_19_: Vec Double
                        """
                        return dbrow_19_
                      def anonymous_fun_63_(row_2_):
                        """
                        row_2_: Vec Double
                        """
                        if row_2_[9] == 0.0:
                          cond_result_20_ = row_2_[9]
                        else:
                          cond_result_20_ = 0.0
                        return cond_result_20_
                      def anonymous_fun_64_(dbrow_20_):
                        """
                        dbrow_20_: Vec Double
                        """
                        return dbrow_20_
                      return ((fun_comp(anonymous_fun_61_,anonymous_fun_62_))(par_map_input_9_),(fun_comp(anonymous_fun_63_,anonymous_fun_64_))(par_map_input_9_))
                    return (anonymous_fun_55_(par_map_input_10_),anonymous_fun_60_(par_map_input_10_))
                  return (anonymous_fun_49_(par_map_input_11_),anonymous_fun_54_(par_map_input_11_))
                return (anonymous_fun_43_(par_map_input_12_),anonymous_fun_48_(par_map_input_12_))
              return (anonymous_fun_37_(par_map_input_13_),anonymous_fun_42_(par_map_input_13_))
            return (anonymous_fun_31_(par_map_input_14_),anonymous_fun_36_(par_map_input_14_))
          return (anonymous_fun_25_(par_map_input_15_),anonymous_fun_30_(par_map_input_15_))
        return (anonymous_fun_19_(par_map_input_16_),anonymous_fun_24_(par_map_input_16_))
      return (anonymous_fun_13_(par_map_input_17_),anonymous_fun_18_(par_map_input_17_))
    return (anonymous_fun_7_(par_map_input_18_),anonymous_fun_12_(par_map_input_18_))
  return anonymous_fun_6_
def anonymous_fun_65_(empty_closure_3_):
  """
  empty_closure_3_: ()
  """
  def anonymous_fun_66_(par_release_input_18_):
    """
    par_release_input_18_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double))))))))))
    """
    def anonymous_fun_67_(par_release_input_0_):
      """
      par_release_input_0_: (Double,Double)
      """
      def anonymous_fun_68_(orange_input_1_):
        """
        orange_input_1_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_1_)
      def anonymous_fun_69_(orange_input_2_):
        """
        orange_input_2_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_2_)
      return (anonymous_fun_68_(par_release_input_0_[0]),anonymous_fun_69_(par_release_input_0_[1]))
    def anonymous_fun_70_(par_release_input_17_):
      """
      par_release_input_17_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))))))
      """
      def anonymous_fun_71_(par_release_input_1_):
        """
        par_release_input_1_: (Double,Double)
        """
        def anonymous_fun_72_(orange_input_3_):
          """
          orange_input_3_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_3_)
        def anonymous_fun_73_(orange_input_4_):
          """
          orange_input_4_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_4_)
        return (anonymous_fun_72_(par_release_input_1_[0]),anonymous_fun_73_(par_release_input_1_[1]))
      def anonymous_fun_74_(par_release_input_16_):
        """
        par_release_input_16_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double))))))))
        """
        def anonymous_fun_75_(par_release_input_2_):
          """
          par_release_input_2_: (Double,Double)
          """
          def anonymous_fun_76_(orange_input_5_):
            """
            orange_input_5_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_5_)
          def anonymous_fun_77_(orange_input_6_):
            """
            orange_input_6_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_6_)
          return (anonymous_fun_76_(par_release_input_2_[0]),anonymous_fun_77_(par_release_input_2_[1]))
        def anonymous_fun_78_(par_release_input_15_):
          """
          par_release_input_15_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))))
          """
          def anonymous_fun_79_(par_release_input_3_):
            """
            par_release_input_3_: (Double,Double)
            """
            def anonymous_fun_80_(orange_input_7_):
              """
              orange_input_7_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_7_)
            def anonymous_fun_81_(orange_input_8_):
              """
              orange_input_8_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_8_)
            return (anonymous_fun_80_(par_release_input_3_[0]),anonymous_fun_81_(par_release_input_3_[1]))
          def anonymous_fun_82_(par_release_input_14_):
            """
            par_release_input_14_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double))))))
            """
            def anonymous_fun_83_(par_release_input_4_):
              """
              par_release_input_4_: (Double,Double)
              """
              def anonymous_fun_84_(orange_input_9_):
                """
                orange_input_9_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_9_)
              def anonymous_fun_85_(orange_input_10_):
                """
                orange_input_10_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_10_)
              return (anonymous_fun_84_(par_release_input_4_[0]),anonymous_fun_85_(par_release_input_4_[1]))
            def anonymous_fun_86_(par_release_input_13_):
              """
              par_release_input_13_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))
              """
              def anonymous_fun_87_(par_release_input_5_):
                """
                par_release_input_5_: (Double,Double)
                """
                def anonymous_fun_88_(orange_input_11_):
                  """
                  orange_input_11_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_11_)
                def anonymous_fun_89_(orange_input_12_):
                  """
                  orange_input_12_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_12_)
                return (anonymous_fun_88_(par_release_input_5_[0]),anonymous_fun_89_(par_release_input_5_[1]))
              def anonymous_fun_90_(par_release_input_12_):
                """
                par_release_input_12_: ((Double,Double),((Double,Double),((Double,Double),(Double,Double))))
                """
                def anonymous_fun_91_(par_release_input_6_):
                  """
                  par_release_input_6_: (Double,Double)
                  """
                  def anonymous_fun_92_(orange_input_13_):
                    """
                    orange_input_13_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_13_)
                  def anonymous_fun_93_(orange_input_14_):
                    """
                    orange_input_14_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_14_)
                  return (anonymous_fun_92_(par_release_input_6_[0]),anonymous_fun_93_(par_release_input_6_[1]))
                def anonymous_fun_94_(par_release_input_11_):
                  """
                  par_release_input_11_: ((Double,Double),((Double,Double),(Double,Double)))
                  """
                  def anonymous_fun_95_(par_release_input_7_):
                    """
                    par_release_input_7_: (Double,Double)
                    """
                    def anonymous_fun_96_(orange_input_15_):
                      """
                      orange_input_15_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_15_)
                    def anonymous_fun_97_(orange_input_16_):
                      """
                      orange_input_16_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_16_)
                    return (anonymous_fun_96_(par_release_input_7_[0]),anonymous_fun_97_(par_release_input_7_[1]))
                  def anonymous_fun_98_(par_release_input_10_):
                    """
                    par_release_input_10_: ((Double,Double),(Double,Double))
                    """
                    def anonymous_fun_99_(par_release_input_8_):
                      """
                      par_release_input_8_: (Double,Double)
                      """
                      def anonymous_fun_100_(orange_input_17_):
                        """
                        orange_input_17_: Double
                        """
                        return laplace_fx(cfix(1.0),orange_input_17_)
                      def anonymous_fun_101_(orange_input_18_):
                        """
                        orange_input_18_: Double
                        """
                        return laplace_fx(cfix(1.0),orange_input_18_)
                      return (anonymous_fun_100_(par_release_input_8_[0]),anonymous_fun_101_(par_release_input_8_[1]))
                    def anonymous_fun_102_(par_release_input_9_):
                      """
                      par_release_input_9_: (Double,Double)
                      """
                      def anonymous_fun_103_(orange_input_19_):
                        """
                        orange_input_19_: Double
                        """
                        return laplace_fx(cfix(1.0),orange_input_19_)
                      def anonymous_fun_104_(orange_input_20_):
                        """
                        orange_input_20_: Double
                        """
                        return laplace_fx(cfix(1.0),orange_input_20_)
                      return (anonymous_fun_103_(par_release_input_9_[0]),anonymous_fun_104_(par_release_input_9_[1]))
                    return (anonymous_fun_99_(par_release_input_10_[0]),anonymous_fun_102_(par_release_input_10_[1]))
                  return (anonymous_fun_95_(par_release_input_11_[0]),anonymous_fun_98_(par_release_input_11_[1]))
                return (anonymous_fun_91_(par_release_input_12_[0]),anonymous_fun_94_(par_release_input_12_[1]))
              return (anonymous_fun_87_(par_release_input_13_[0]),anonymous_fun_90_(par_release_input_13_[1]))
            return (anonymous_fun_83_(par_release_input_14_[0]),anonymous_fun_86_(par_release_input_14_[1]))
          return (anonymous_fun_79_(par_release_input_15_[0]),anonymous_fun_82_(par_release_input_15_[1]))
        return (anonymous_fun_75_(par_release_input_16_[0]),anonymous_fun_78_(par_release_input_16_[1]))
      return (anonymous_fun_71_(par_release_input_17_[0]),anonymous_fun_74_(par_release_input_17_[1]))
    return (anonymous_fun_67_(par_release_input_18_[0]),anonymous_fun_70_(par_release_input_18_[1]))
  return anonymous_fun_66_
fused_vector_tup_0_ = bmcs(20,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_5_,(),anonymous_fun_65_)
noised_sum_pairs_0_ = [fused_vector_tup_0_[0],(fused_vector_tup_0_[1])[0],((fused_vector_tup_0_[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[0],((((fused_vector_tup_0_[1])[1])[1])[1])[0],(((((fused_vector_tup_0_[1])[1])[1])[1])[1])[0],((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[0],(((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1]]
def anonymous_fun_105_(curr_acc_0_):
  """
  curr_acc_0_: Vec Double
  """
  return len(curr_acc_0_) < len(noised_sum_pairs_0_)
def anonymous_fun_106_(curr_acc_1_):
  """
  curr_acc_1_: Vec Double
  """
  return curr_acc_1_ + [math.log(((noised_sum_pairs_0_[len(curr_acc_1_)])[0] / noised_label_sum_0_) / ((noised_sum_pairs_0_[len(curr_acc_1_)])[1] / noised_label_sum_0_)) - math.log((1.0 - (noised_sum_pairs_0_[len(curr_acc_1_)])[0] / noised_label_sum_0_) / (1.0 - (noised_sum_pairs_0_[len(curr_acc_1_)])[1] / noised_label_sum_0_))]
loop_acc_0_ = []
while anonymous_fun_105_(loop_acc_0_):
  loop_acc_0_ = anonymous_fun_106_(loop_acc_0_)
loop_acc_0_