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
    row_0_: ((Vec Double),Bool)
    """
    return 1.0
  def anonymous_fun_2_(dbrow_0_):
    """
    dbrow_0_: ((Vec Double),Bool)
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
noisy_count_0_ = bmcs(1,[1.0],(),anonymous_fun_0_,(),anonymous_fun_3_)
if (noisy_count_0_ / 7.0) / 2.0 < 1.4142135623730951:
  cond_result_0_ = None
else:
  def anonymous_fun_5_(empty_closure_2_):
    """
    empty_closure_2_: ()
    """
    def anonymous_fun_6_(par_map_input_2_):
      """
      par_map_input_2_: ((Vec Double),Bool)
      """
      def anonymous_fun_7_(par_map_input_0_):
        """
        par_map_input_0_: ((Vec Double),Bool)
        """
        def anonymous_fun_8_(row_3_):
          """
          row_3_: ((Vec Double),Bool)
          """
          if (row_3_[0])[0] == 0.0:
            cond_result_1_ = 1.0
          else:
            cond_result_1_ = 0.0
          return cond_result_1_
        def anonymous_fun_9_(dbrow_1_):
          """
          dbrow_1_: ((Vec Double),Bool)
          """
          return dbrow_1_
        def anonymous_fun_10_(row_4_):
          """
          row_4_: ((Vec Double),Bool)
          """
          if row_4_[1] and (row_4_[0])[0] == 0.0:
            cond_result_2_ = 1.0
          else:
            cond_result_2_ = 0.0
          return cond_result_2_
        def anonymous_fun_11_(dbrow_2_):
          """
          dbrow_2_: ((Vec Double),Bool)
          """
          return dbrow_2_
        return ((fun_comp(anonymous_fun_8_,anonymous_fun_9_))(par_map_input_0_),(fun_comp(anonymous_fun_10_,anonymous_fun_11_))(par_map_input_0_))
      def anonymous_fun_12_(par_map_input_1_):
        """
        par_map_input_1_: ((Vec Double),Bool)
        """
        def anonymous_fun_13_(row_1_):
          """
          row_1_: ((Vec Double),Bool)
          """
          if (row_1_[0])[0] == 1.0:
            cond_result_3_ = 1.0
          else:
            cond_result_3_ = 0.0
          return cond_result_3_
        def anonymous_fun_14_(dbrow_3_):
          """
          dbrow_3_: ((Vec Double),Bool)
          """
          return dbrow_3_
        def anonymous_fun_15_(row_2_):
          """
          row_2_: ((Vec Double),Bool)
          """
          if row_2_[1] and (row_2_[0])[0] == 1.0:
            cond_result_4_ = 1.0
          else:
            cond_result_4_ = 0.0
          return cond_result_4_
        def anonymous_fun_16_(dbrow_4_):
          """
          dbrow_4_: ((Vec Double),Bool)
          """
          return dbrow_4_
        return ((fun_comp(anonymous_fun_13_,anonymous_fun_14_))(par_map_input_1_),(fun_comp(anonymous_fun_15_,anonymous_fun_16_))(par_map_input_1_))
      return (anonymous_fun_7_(par_map_input_2_),anonymous_fun_12_(par_map_input_2_))
    return anonymous_fun_6_
  def anonymous_fun_17_(empty_closure_3_):
    """
    empty_closure_3_: ()
    """
    def anonymous_fun_18_(par_release_input_2_):
      """
      par_release_input_2_: ((Double,Double),(Double,Double))
      """
      def anonymous_fun_19_(par_release_input_0_):
        """
        par_release_input_0_: (Double,Double)
        """
        def anonymous_fun_20_(orange_input_1_):
          """
          orange_input_1_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_1_)
        def anonymous_fun_21_(orange_input_2_):
          """
          orange_input_2_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_2_)
        return (anonymous_fun_20_(par_release_input_0_[0]),anonymous_fun_21_(par_release_input_0_[1]))
      def anonymous_fun_22_(par_release_input_1_):
        """
        par_release_input_1_: (Double,Double)
        """
        def anonymous_fun_23_(orange_input_3_):
          """
          orange_input_3_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_3_)
        def anonymous_fun_24_(orange_input_4_):
          """
          orange_input_4_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_4_)
        return (anonymous_fun_23_(par_release_input_1_[0]),anonymous_fun_24_(par_release_input_1_[1]))
      return (anonymous_fun_19_(par_release_input_2_[0]),anonymous_fun_22_(par_release_input_2_[1]))
    return anonymous_fun_18_
  fused_vector_tup_0_ = bmcs(4,[1.0,1.0,1.0,1.0],(),anonymous_fun_5_,(),anonymous_fun_17_)
  fused_samples_0_ = [fused_vector_tup_0_[0],fused_vector_tup_0_[1]]
  def anonymous_fun_25_(empty_closure_4_):
    """
    empty_closure_4_: ()
    """
    def anonymous_fun_26_(par_map_input_7_):
      """
      par_map_input_7_: ((Vec Double),Bool)
      """
      def anonymous_fun_27_(par_map_input_3_):
        """
        par_map_input_3_: ((Vec Double),Bool)
        """
        def anonymous_fun_28_(row_9_):
          """
          row_9_: ((Vec Double),Bool)
          """
          if (row_9_[0])[1] == 0.0:
            cond_result_5_ = 1.0
          else:
            cond_result_5_ = 0.0
          return cond_result_5_
        def anonymous_fun_29_(dbrow_5_):
          """
          dbrow_5_: ((Vec Double),Bool)
          """
          return dbrow_5_
        def anonymous_fun_30_(row_10_):
          """
          row_10_: ((Vec Double),Bool)
          """
          if row_10_[1] and (row_10_[0])[1] == 0.0:
            cond_result_6_ = 1.0
          else:
            cond_result_6_ = 0.0
          return cond_result_6_
        def anonymous_fun_31_(dbrow_6_):
          """
          dbrow_6_: ((Vec Double),Bool)
          """
          return dbrow_6_
        return ((fun_comp(anonymous_fun_28_,anonymous_fun_29_))(par_map_input_3_),(fun_comp(anonymous_fun_30_,anonymous_fun_31_))(par_map_input_3_))
      def anonymous_fun_32_(par_map_input_6_):
        """
        par_map_input_6_: ((Vec Double),Bool)
        """
        def anonymous_fun_33_(par_map_input_4_):
          """
          par_map_input_4_: ((Vec Double),Bool)
          """
          def anonymous_fun_34_(row_7_):
            """
            row_7_: ((Vec Double),Bool)
            """
            if (row_7_[0])[1] == 1.0:
              cond_result_7_ = 1.0
            else:
              cond_result_7_ = 0.0
            return cond_result_7_
          def anonymous_fun_35_(dbrow_7_):
            """
            dbrow_7_: ((Vec Double),Bool)
            """
            return dbrow_7_
          def anonymous_fun_36_(row_8_):
            """
            row_8_: ((Vec Double),Bool)
            """
            if row_8_[1] and (row_8_[0])[1] == 1.0:
              cond_result_8_ = 1.0
            else:
              cond_result_8_ = 0.0
            return cond_result_8_
          def anonymous_fun_37_(dbrow_8_):
            """
            dbrow_8_: ((Vec Double),Bool)
            """
            return dbrow_8_
          return ((fun_comp(anonymous_fun_34_,anonymous_fun_35_))(par_map_input_4_),(fun_comp(anonymous_fun_36_,anonymous_fun_37_))(par_map_input_4_))
        def anonymous_fun_38_(par_map_input_5_):
          """
          par_map_input_5_: ((Vec Double),Bool)
          """
          def anonymous_fun_39_(row_5_):
            """
            row_5_: ((Vec Double),Bool)
            """
            if (row_5_[0])[1] == 2.0:
              cond_result_9_ = 1.0
            else:
              cond_result_9_ = 0.0
            return cond_result_9_
          def anonymous_fun_40_(dbrow_9_):
            """
            dbrow_9_: ((Vec Double),Bool)
            """
            return dbrow_9_
          def anonymous_fun_41_(row_6_):
            """
            row_6_: ((Vec Double),Bool)
            """
            if row_6_[1] and (row_6_[0])[1] == 2.0:
              cond_result_10_ = 1.0
            else:
              cond_result_10_ = 0.0
            return cond_result_10_
          def anonymous_fun_42_(dbrow_10_):
            """
            dbrow_10_: ((Vec Double),Bool)
            """
            return dbrow_10_
          return ((fun_comp(anonymous_fun_39_,anonymous_fun_40_))(par_map_input_5_),(fun_comp(anonymous_fun_41_,anonymous_fun_42_))(par_map_input_5_))
        return (anonymous_fun_33_(par_map_input_6_),anonymous_fun_38_(par_map_input_6_))
      return (anonymous_fun_27_(par_map_input_7_),anonymous_fun_32_(par_map_input_7_))
    return anonymous_fun_26_
  def anonymous_fun_43_(empty_closure_5_):
    """
    empty_closure_5_: ()
    """
    def anonymous_fun_44_(par_release_input_7_):
      """
      par_release_input_7_: ((Double,Double),((Double,Double),(Double,Double)))
      """
      def anonymous_fun_45_(par_release_input_3_):
        """
        par_release_input_3_: (Double,Double)
        """
        def anonymous_fun_46_(orange_input_5_):
          """
          orange_input_5_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_5_)
        def anonymous_fun_47_(orange_input_6_):
          """
          orange_input_6_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_6_)
        return (anonymous_fun_46_(par_release_input_3_[0]),anonymous_fun_47_(par_release_input_3_[1]))
      def anonymous_fun_48_(par_release_input_6_):
        """
        par_release_input_6_: ((Double,Double),(Double,Double))
        """
        def anonymous_fun_49_(par_release_input_4_):
          """
          par_release_input_4_: (Double,Double)
          """
          def anonymous_fun_50_(orange_input_7_):
            """
            orange_input_7_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_7_)
          def anonymous_fun_51_(orange_input_8_):
            """
            orange_input_8_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_8_)
          return (anonymous_fun_50_(par_release_input_4_[0]),anonymous_fun_51_(par_release_input_4_[1]))
        def anonymous_fun_52_(par_release_input_5_):
          """
          par_release_input_5_: (Double,Double)
          """
          def anonymous_fun_53_(orange_input_9_):
            """
            orange_input_9_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_9_)
          def anonymous_fun_54_(orange_input_10_):
            """
            orange_input_10_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_10_)
          return (anonymous_fun_53_(par_release_input_5_[0]),anonymous_fun_54_(par_release_input_5_[1]))
        return (anonymous_fun_49_(par_release_input_6_[0]),anonymous_fun_52_(par_release_input_6_[1]))
      return (anonymous_fun_45_(par_release_input_7_[0]),anonymous_fun_48_(par_release_input_7_[1]))
    return anonymous_fun_44_
  fused_vector_tup_1_ = bmcs(6,[1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_25_,(),anonymous_fun_43_)
  fused_samples_1_ = [fused_vector_tup_1_[0],(fused_vector_tup_1_[1])[0],(fused_vector_tup_1_[1])[1]]
  def anonymous_fun_55_(empty_closure_6_):
    """
    empty_closure_6_: ()
    """
    def anonymous_fun_56_(par_map_input_14_):
      """
      par_map_input_14_: ((Vec Double),Bool)
      """
      def anonymous_fun_57_(par_map_input_8_):
        """
        par_map_input_8_: ((Vec Double),Bool)
        """
        def anonymous_fun_58_(row_17_):
          """
          row_17_: ((Vec Double),Bool)
          """
          if (row_17_[0])[2] == 0.0:
            cond_result_11_ = 1.0
          else:
            cond_result_11_ = 0.0
          return cond_result_11_
        def anonymous_fun_59_(dbrow_11_):
          """
          dbrow_11_: ((Vec Double),Bool)
          """
          return dbrow_11_
        def anonymous_fun_60_(row_18_):
          """
          row_18_: ((Vec Double),Bool)
          """
          if row_18_[1] and (row_18_[0])[2] == 0.0:
            cond_result_12_ = 1.0
          else:
            cond_result_12_ = 0.0
          return cond_result_12_
        def anonymous_fun_61_(dbrow_12_):
          """
          dbrow_12_: ((Vec Double),Bool)
          """
          return dbrow_12_
        return ((fun_comp(anonymous_fun_58_,anonymous_fun_59_))(par_map_input_8_),(fun_comp(anonymous_fun_60_,anonymous_fun_61_))(par_map_input_8_))
      def anonymous_fun_62_(par_map_input_13_):
        """
        par_map_input_13_: ((Vec Double),Bool)
        """
        def anonymous_fun_63_(par_map_input_9_):
          """
          par_map_input_9_: ((Vec Double),Bool)
          """
          def anonymous_fun_64_(row_15_):
            """
            row_15_: ((Vec Double),Bool)
            """
            if (row_15_[0])[2] == 1.0:
              cond_result_13_ = 1.0
            else:
              cond_result_13_ = 0.0
            return cond_result_13_
          def anonymous_fun_65_(dbrow_13_):
            """
            dbrow_13_: ((Vec Double),Bool)
            """
            return dbrow_13_
          def anonymous_fun_66_(row_16_):
            """
            row_16_: ((Vec Double),Bool)
            """
            if row_16_[1] and (row_16_[0])[2] == 1.0:
              cond_result_14_ = 1.0
            else:
              cond_result_14_ = 0.0
            return cond_result_14_
          def anonymous_fun_67_(dbrow_14_):
            """
            dbrow_14_: ((Vec Double),Bool)
            """
            return dbrow_14_
          return ((fun_comp(anonymous_fun_64_,anonymous_fun_65_))(par_map_input_9_),(fun_comp(anonymous_fun_66_,anonymous_fun_67_))(par_map_input_9_))
        def anonymous_fun_68_(par_map_input_12_):
          """
          par_map_input_12_: ((Vec Double),Bool)
          """
          def anonymous_fun_69_(par_map_input_10_):
            """
            par_map_input_10_: ((Vec Double),Bool)
            """
            def anonymous_fun_70_(row_13_):
              """
              row_13_: ((Vec Double),Bool)
              """
              if (row_13_[0])[2] == 2.0:
                cond_result_15_ = 1.0
              else:
                cond_result_15_ = 0.0
              return cond_result_15_
            def anonymous_fun_71_(dbrow_15_):
              """
              dbrow_15_: ((Vec Double),Bool)
              """
              return dbrow_15_
            def anonymous_fun_72_(row_14_):
              """
              row_14_: ((Vec Double),Bool)
              """
              if row_14_[1] and (row_14_[0])[2] == 2.0:
                cond_result_16_ = 1.0
              else:
                cond_result_16_ = 0.0
              return cond_result_16_
            def anonymous_fun_73_(dbrow_16_):
              """
              dbrow_16_: ((Vec Double),Bool)
              """
              return dbrow_16_
            return ((fun_comp(anonymous_fun_70_,anonymous_fun_71_))(par_map_input_10_),(fun_comp(anonymous_fun_72_,anonymous_fun_73_))(par_map_input_10_))
          def anonymous_fun_74_(par_map_input_11_):
            """
            par_map_input_11_: ((Vec Double),Bool)
            """
            def anonymous_fun_75_(row_11_):
              """
              row_11_: ((Vec Double),Bool)
              """
              if (row_11_[0])[2] == 3.0:
                cond_result_17_ = 1.0
              else:
                cond_result_17_ = 0.0
              return cond_result_17_
            def anonymous_fun_76_(dbrow_17_):
              """
              dbrow_17_: ((Vec Double),Bool)
              """
              return dbrow_17_
            def anonymous_fun_77_(row_12_):
              """
              row_12_: ((Vec Double),Bool)
              """
              if row_12_[1] and (row_12_[0])[2] == 3.0:
                cond_result_18_ = 1.0
              else:
                cond_result_18_ = 0.0
              return cond_result_18_
            def anonymous_fun_78_(dbrow_18_):
              """
              dbrow_18_: ((Vec Double),Bool)
              """
              return dbrow_18_
            return ((fun_comp(anonymous_fun_75_,anonymous_fun_76_))(par_map_input_11_),(fun_comp(anonymous_fun_77_,anonymous_fun_78_))(par_map_input_11_))
          return (anonymous_fun_69_(par_map_input_12_),anonymous_fun_74_(par_map_input_12_))
        return (anonymous_fun_63_(par_map_input_13_),anonymous_fun_68_(par_map_input_13_))
      return (anonymous_fun_57_(par_map_input_14_),anonymous_fun_62_(par_map_input_14_))
    return anonymous_fun_56_
  def anonymous_fun_79_(empty_closure_7_):
    """
    empty_closure_7_: ()
    """
    def anonymous_fun_80_(par_release_input_14_):
      """
      par_release_input_14_: ((Double,Double),((Double,Double),((Double,Double),(Double,Double))))
      """
      def anonymous_fun_81_(par_release_input_8_):
        """
        par_release_input_8_: (Double,Double)
        """
        def anonymous_fun_82_(orange_input_11_):
          """
          orange_input_11_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_11_)
        def anonymous_fun_83_(orange_input_12_):
          """
          orange_input_12_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_12_)
        return (anonymous_fun_82_(par_release_input_8_[0]),anonymous_fun_83_(par_release_input_8_[1]))
      def anonymous_fun_84_(par_release_input_13_):
        """
        par_release_input_13_: ((Double,Double),((Double,Double),(Double,Double)))
        """
        def anonymous_fun_85_(par_release_input_9_):
          """
          par_release_input_9_: (Double,Double)
          """
          def anonymous_fun_86_(orange_input_13_):
            """
            orange_input_13_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_13_)
          def anonymous_fun_87_(orange_input_14_):
            """
            orange_input_14_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_14_)
          return (anonymous_fun_86_(par_release_input_9_[0]),anonymous_fun_87_(par_release_input_9_[1]))
        def anonymous_fun_88_(par_release_input_12_):
          """
          par_release_input_12_: ((Double,Double),(Double,Double))
          """
          def anonymous_fun_89_(par_release_input_10_):
            """
            par_release_input_10_: (Double,Double)
            """
            def anonymous_fun_90_(orange_input_15_):
              """
              orange_input_15_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_15_)
            def anonymous_fun_91_(orange_input_16_):
              """
              orange_input_16_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_16_)
            return (anonymous_fun_90_(par_release_input_10_[0]),anonymous_fun_91_(par_release_input_10_[1]))
          def anonymous_fun_92_(par_release_input_11_):
            """
            par_release_input_11_: (Double,Double)
            """
            def anonymous_fun_93_(orange_input_17_):
              """
              orange_input_17_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_17_)
            def anonymous_fun_94_(orange_input_18_):
              """
              orange_input_18_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_18_)
            return (anonymous_fun_93_(par_release_input_11_[0]),anonymous_fun_94_(par_release_input_11_[1]))
          return (anonymous_fun_89_(par_release_input_12_[0]),anonymous_fun_92_(par_release_input_12_[1]))
        return (anonymous_fun_85_(par_release_input_13_[0]),anonymous_fun_88_(par_release_input_13_[1]))
      return (anonymous_fun_81_(par_release_input_14_[0]),anonymous_fun_84_(par_release_input_14_[1]))
    return anonymous_fun_80_
  fused_vector_tup_2_ = bmcs(8,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_55_,(),anonymous_fun_79_)
  fused_samples_2_ = [fused_vector_tup_2_[0],(fused_vector_tup_2_[1])[0],((fused_vector_tup_2_[1])[1])[0],((fused_vector_tup_2_[1])[1])[1]]
  def anonymous_fun_95_(empty_closure_8_):
    """
    empty_closure_8_: ()
    """
    def anonymous_fun_96_(par_map_input_23_):
      """
      par_map_input_23_: ((Vec Double),Bool)
      """
      def anonymous_fun_97_(par_map_input_15_):
        """
        par_map_input_15_: ((Vec Double),Bool)
        """
        def anonymous_fun_98_(row_27_):
          """
          row_27_: ((Vec Double),Bool)
          """
          if (row_27_[0])[3] == 0.0:
            cond_result_19_ = 1.0
          else:
            cond_result_19_ = 0.0
          return cond_result_19_
        def anonymous_fun_99_(dbrow_19_):
          """
          dbrow_19_: ((Vec Double),Bool)
          """
          return dbrow_19_
        def anonymous_fun_100_(row_28_):
          """
          row_28_: ((Vec Double),Bool)
          """
          if row_28_[1] and (row_28_[0])[3] == 0.0:
            cond_result_20_ = 1.0
          else:
            cond_result_20_ = 0.0
          return cond_result_20_
        def anonymous_fun_101_(dbrow_20_):
          """
          dbrow_20_: ((Vec Double),Bool)
          """
          return dbrow_20_
        return ((fun_comp(anonymous_fun_98_,anonymous_fun_99_))(par_map_input_15_),(fun_comp(anonymous_fun_100_,anonymous_fun_101_))(par_map_input_15_))
      def anonymous_fun_102_(par_map_input_22_):
        """
        par_map_input_22_: ((Vec Double),Bool)
        """
        def anonymous_fun_103_(par_map_input_16_):
          """
          par_map_input_16_: ((Vec Double),Bool)
          """
          def anonymous_fun_104_(row_25_):
            """
            row_25_: ((Vec Double),Bool)
            """
            if (row_25_[0])[3] == 1.0:
              cond_result_21_ = 1.0
            else:
              cond_result_21_ = 0.0
            return cond_result_21_
          def anonymous_fun_105_(dbrow_21_):
            """
            dbrow_21_: ((Vec Double),Bool)
            """
            return dbrow_21_
          def anonymous_fun_106_(row_26_):
            """
            row_26_: ((Vec Double),Bool)
            """
            if row_26_[1] and (row_26_[0])[3] == 1.0:
              cond_result_22_ = 1.0
            else:
              cond_result_22_ = 0.0
            return cond_result_22_
          def anonymous_fun_107_(dbrow_22_):
            """
            dbrow_22_: ((Vec Double),Bool)
            """
            return dbrow_22_
          return ((fun_comp(anonymous_fun_104_,anonymous_fun_105_))(par_map_input_16_),(fun_comp(anonymous_fun_106_,anonymous_fun_107_))(par_map_input_16_))
        def anonymous_fun_108_(par_map_input_21_):
          """
          par_map_input_21_: ((Vec Double),Bool)
          """
          def anonymous_fun_109_(par_map_input_17_):
            """
            par_map_input_17_: ((Vec Double),Bool)
            """
            def anonymous_fun_110_(row_23_):
              """
              row_23_: ((Vec Double),Bool)
              """
              if (row_23_[0])[3] == 2.0:
                cond_result_23_ = 1.0
              else:
                cond_result_23_ = 0.0
              return cond_result_23_
            def anonymous_fun_111_(dbrow_23_):
              """
              dbrow_23_: ((Vec Double),Bool)
              """
              return dbrow_23_
            def anonymous_fun_112_(row_24_):
              """
              row_24_: ((Vec Double),Bool)
              """
              if row_24_[1] and (row_24_[0])[3] == 2.0:
                cond_result_24_ = 1.0
              else:
                cond_result_24_ = 0.0
              return cond_result_24_
            def anonymous_fun_113_(dbrow_24_):
              """
              dbrow_24_: ((Vec Double),Bool)
              """
              return dbrow_24_
            return ((fun_comp(anonymous_fun_110_,anonymous_fun_111_))(par_map_input_17_),(fun_comp(anonymous_fun_112_,anonymous_fun_113_))(par_map_input_17_))
          def anonymous_fun_114_(par_map_input_20_):
            """
            par_map_input_20_: ((Vec Double),Bool)
            """
            def anonymous_fun_115_(par_map_input_18_):
              """
              par_map_input_18_: ((Vec Double),Bool)
              """
              def anonymous_fun_116_(row_21_):
                """
                row_21_: ((Vec Double),Bool)
                """
                if (row_21_[0])[3] == 3.0:
                  cond_result_25_ = 1.0
                else:
                  cond_result_25_ = 0.0
                return cond_result_25_
              def anonymous_fun_117_(dbrow_25_):
                """
                dbrow_25_: ((Vec Double),Bool)
                """
                return dbrow_25_
              def anonymous_fun_118_(row_22_):
                """
                row_22_: ((Vec Double),Bool)
                """
                if row_22_[1] and (row_22_[0])[3] == 3.0:
                  cond_result_26_ = 1.0
                else:
                  cond_result_26_ = 0.0
                return cond_result_26_
              def anonymous_fun_119_(dbrow_26_):
                """
                dbrow_26_: ((Vec Double),Bool)
                """
                return dbrow_26_
              return ((fun_comp(anonymous_fun_116_,anonymous_fun_117_))(par_map_input_18_),(fun_comp(anonymous_fun_118_,anonymous_fun_119_))(par_map_input_18_))
            def anonymous_fun_120_(par_map_input_19_):
              """
              par_map_input_19_: ((Vec Double),Bool)
              """
              def anonymous_fun_121_(row_19_):
                """
                row_19_: ((Vec Double),Bool)
                """
                if (row_19_[0])[3] == 4.0:
                  cond_result_27_ = 1.0
                else:
                  cond_result_27_ = 0.0
                return cond_result_27_
              def anonymous_fun_122_(dbrow_27_):
                """
                dbrow_27_: ((Vec Double),Bool)
                """
                return dbrow_27_
              def anonymous_fun_123_(row_20_):
                """
                row_20_: ((Vec Double),Bool)
                """
                if row_20_[1] and (row_20_[0])[3] == 4.0:
                  cond_result_28_ = 1.0
                else:
                  cond_result_28_ = 0.0
                return cond_result_28_
              def anonymous_fun_124_(dbrow_28_):
                """
                dbrow_28_: ((Vec Double),Bool)
                """
                return dbrow_28_
              return ((fun_comp(anonymous_fun_121_,anonymous_fun_122_))(par_map_input_19_),(fun_comp(anonymous_fun_123_,anonymous_fun_124_))(par_map_input_19_))
            return (anonymous_fun_115_(par_map_input_20_),anonymous_fun_120_(par_map_input_20_))
          return (anonymous_fun_109_(par_map_input_21_),anonymous_fun_114_(par_map_input_21_))
        return (anonymous_fun_103_(par_map_input_22_),anonymous_fun_108_(par_map_input_22_))
      return (anonymous_fun_97_(par_map_input_23_),anonymous_fun_102_(par_map_input_23_))
    return anonymous_fun_96_
  def anonymous_fun_125_(empty_closure_9_):
    """
    empty_closure_9_: ()
    """
    def anonymous_fun_126_(par_release_input_23_):
      """
      par_release_input_23_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))
      """
      def anonymous_fun_127_(par_release_input_15_):
        """
        par_release_input_15_: (Double,Double)
        """
        def anonymous_fun_128_(orange_input_19_):
          """
          orange_input_19_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_19_)
        def anonymous_fun_129_(orange_input_20_):
          """
          orange_input_20_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_20_)
        return (anonymous_fun_128_(par_release_input_15_[0]),anonymous_fun_129_(par_release_input_15_[1]))
      def anonymous_fun_130_(par_release_input_22_):
        """
        par_release_input_22_: ((Double,Double),((Double,Double),((Double,Double),(Double,Double))))
        """
        def anonymous_fun_131_(par_release_input_16_):
          """
          par_release_input_16_: (Double,Double)
          """
          def anonymous_fun_132_(orange_input_21_):
            """
            orange_input_21_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_21_)
          def anonymous_fun_133_(orange_input_22_):
            """
            orange_input_22_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_22_)
          return (anonymous_fun_132_(par_release_input_16_[0]),anonymous_fun_133_(par_release_input_16_[1]))
        def anonymous_fun_134_(par_release_input_21_):
          """
          par_release_input_21_: ((Double,Double),((Double,Double),(Double,Double)))
          """
          def anonymous_fun_135_(par_release_input_17_):
            """
            par_release_input_17_: (Double,Double)
            """
            def anonymous_fun_136_(orange_input_23_):
              """
              orange_input_23_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_23_)
            def anonymous_fun_137_(orange_input_24_):
              """
              orange_input_24_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_24_)
            return (anonymous_fun_136_(par_release_input_17_[0]),anonymous_fun_137_(par_release_input_17_[1]))
          def anonymous_fun_138_(par_release_input_20_):
            """
            par_release_input_20_: ((Double,Double),(Double,Double))
            """
            def anonymous_fun_139_(par_release_input_18_):
              """
              par_release_input_18_: (Double,Double)
              """
              def anonymous_fun_140_(orange_input_25_):
                """
                orange_input_25_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_25_)
              def anonymous_fun_141_(orange_input_26_):
                """
                orange_input_26_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_26_)
              return (anonymous_fun_140_(par_release_input_18_[0]),anonymous_fun_141_(par_release_input_18_[1]))
            def anonymous_fun_142_(par_release_input_19_):
              """
              par_release_input_19_: (Double,Double)
              """
              def anonymous_fun_143_(orange_input_27_):
                """
                orange_input_27_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_27_)
              def anonymous_fun_144_(orange_input_28_):
                """
                orange_input_28_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_28_)
              return (anonymous_fun_143_(par_release_input_19_[0]),anonymous_fun_144_(par_release_input_19_[1]))
            return (anonymous_fun_139_(par_release_input_20_[0]),anonymous_fun_142_(par_release_input_20_[1]))
          return (anonymous_fun_135_(par_release_input_21_[0]),anonymous_fun_138_(par_release_input_21_[1]))
        return (anonymous_fun_131_(par_release_input_22_[0]),anonymous_fun_134_(par_release_input_22_[1]))
      return (anonymous_fun_127_(par_release_input_23_[0]),anonymous_fun_130_(par_release_input_23_[1]))
    return anonymous_fun_126_
  fused_vector_tup_3_ = bmcs(10,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_95_,(),anonymous_fun_125_)
  fused_samples_3_ = [fused_vector_tup_3_[0],(fused_vector_tup_3_[1])[0],((fused_vector_tup_3_[1])[1])[0],(((fused_vector_tup_3_[1])[1])[1])[0],(((fused_vector_tup_3_[1])[1])[1])[1]]
  def anonymous_fun_145_(empty_closure_10_):
    """
    empty_closure_10_: ()
    """
    def anonymous_fun_146_(par_map_input_34_):
      """
      par_map_input_34_: ((Vec Double),Bool)
      """
      def anonymous_fun_147_(par_map_input_24_):
        """
        par_map_input_24_: ((Vec Double),Bool)
        """
        def anonymous_fun_148_(row_39_):
          """
          row_39_: ((Vec Double),Bool)
          """
          if (row_39_[0])[4] == 0.0:
            cond_result_29_ = 1.0
          else:
            cond_result_29_ = 0.0
          return cond_result_29_
        def anonymous_fun_149_(dbrow_29_):
          """
          dbrow_29_: ((Vec Double),Bool)
          """
          return dbrow_29_
        def anonymous_fun_150_(row_40_):
          """
          row_40_: ((Vec Double),Bool)
          """
          if row_40_[1] and (row_40_[0])[4] == 0.0:
            cond_result_30_ = 1.0
          else:
            cond_result_30_ = 0.0
          return cond_result_30_
        def anonymous_fun_151_(dbrow_30_):
          """
          dbrow_30_: ((Vec Double),Bool)
          """
          return dbrow_30_
        return ((fun_comp(anonymous_fun_148_,anonymous_fun_149_))(par_map_input_24_),(fun_comp(anonymous_fun_150_,anonymous_fun_151_))(par_map_input_24_))
      def anonymous_fun_152_(par_map_input_33_):
        """
        par_map_input_33_: ((Vec Double),Bool)
        """
        def anonymous_fun_153_(par_map_input_25_):
          """
          par_map_input_25_: ((Vec Double),Bool)
          """
          def anonymous_fun_154_(row_37_):
            """
            row_37_: ((Vec Double),Bool)
            """
            if (row_37_[0])[4] == 1.0:
              cond_result_31_ = 1.0
            else:
              cond_result_31_ = 0.0
            return cond_result_31_
          def anonymous_fun_155_(dbrow_31_):
            """
            dbrow_31_: ((Vec Double),Bool)
            """
            return dbrow_31_
          def anonymous_fun_156_(row_38_):
            """
            row_38_: ((Vec Double),Bool)
            """
            if row_38_[1] and (row_38_[0])[4] == 1.0:
              cond_result_32_ = 1.0
            else:
              cond_result_32_ = 0.0
            return cond_result_32_
          def anonymous_fun_157_(dbrow_32_):
            """
            dbrow_32_: ((Vec Double),Bool)
            """
            return dbrow_32_
          return ((fun_comp(anonymous_fun_154_,anonymous_fun_155_))(par_map_input_25_),(fun_comp(anonymous_fun_156_,anonymous_fun_157_))(par_map_input_25_))
        def anonymous_fun_158_(par_map_input_32_):
          """
          par_map_input_32_: ((Vec Double),Bool)
          """
          def anonymous_fun_159_(par_map_input_26_):
            """
            par_map_input_26_: ((Vec Double),Bool)
            """
            def anonymous_fun_160_(row_35_):
              """
              row_35_: ((Vec Double),Bool)
              """
              if (row_35_[0])[4] == 2.0:
                cond_result_33_ = 1.0
              else:
                cond_result_33_ = 0.0
              return cond_result_33_
            def anonymous_fun_161_(dbrow_33_):
              """
              dbrow_33_: ((Vec Double),Bool)
              """
              return dbrow_33_
            def anonymous_fun_162_(row_36_):
              """
              row_36_: ((Vec Double),Bool)
              """
              if row_36_[1] and (row_36_[0])[4] == 2.0:
                cond_result_34_ = 1.0
              else:
                cond_result_34_ = 0.0
              return cond_result_34_
            def anonymous_fun_163_(dbrow_34_):
              """
              dbrow_34_: ((Vec Double),Bool)
              """
              return dbrow_34_
            return ((fun_comp(anonymous_fun_160_,anonymous_fun_161_))(par_map_input_26_),(fun_comp(anonymous_fun_162_,anonymous_fun_163_))(par_map_input_26_))
          def anonymous_fun_164_(par_map_input_31_):
            """
            par_map_input_31_: ((Vec Double),Bool)
            """
            def anonymous_fun_165_(par_map_input_27_):
              """
              par_map_input_27_: ((Vec Double),Bool)
              """
              def anonymous_fun_166_(row_33_):
                """
                row_33_: ((Vec Double),Bool)
                """
                if (row_33_[0])[4] == 3.0:
                  cond_result_35_ = 1.0
                else:
                  cond_result_35_ = 0.0
                return cond_result_35_
              def anonymous_fun_167_(dbrow_35_):
                """
                dbrow_35_: ((Vec Double),Bool)
                """
                return dbrow_35_
              def anonymous_fun_168_(row_34_):
                """
                row_34_: ((Vec Double),Bool)
                """
                if row_34_[1] and (row_34_[0])[4] == 3.0:
                  cond_result_36_ = 1.0
                else:
                  cond_result_36_ = 0.0
                return cond_result_36_
              def anonymous_fun_169_(dbrow_36_):
                """
                dbrow_36_: ((Vec Double),Bool)
                """
                return dbrow_36_
              return ((fun_comp(anonymous_fun_166_,anonymous_fun_167_))(par_map_input_27_),(fun_comp(anonymous_fun_168_,anonymous_fun_169_))(par_map_input_27_))
            def anonymous_fun_170_(par_map_input_30_):
              """
              par_map_input_30_: ((Vec Double),Bool)
              """
              def anonymous_fun_171_(par_map_input_28_):
                """
                par_map_input_28_: ((Vec Double),Bool)
                """
                def anonymous_fun_172_(row_31_):
                  """
                  row_31_: ((Vec Double),Bool)
                  """
                  if (row_31_[0])[4] == 4.0:
                    cond_result_37_ = 1.0
                  else:
                    cond_result_37_ = 0.0
                  return cond_result_37_
                def anonymous_fun_173_(dbrow_37_):
                  """
                  dbrow_37_: ((Vec Double),Bool)
                  """
                  return dbrow_37_
                def anonymous_fun_174_(row_32_):
                  """
                  row_32_: ((Vec Double),Bool)
                  """
                  if row_32_[1] and (row_32_[0])[4] == 4.0:
                    cond_result_38_ = 1.0
                  else:
                    cond_result_38_ = 0.0
                  return cond_result_38_
                def anonymous_fun_175_(dbrow_38_):
                  """
                  dbrow_38_: ((Vec Double),Bool)
                  """
                  return dbrow_38_
                return ((fun_comp(anonymous_fun_172_,anonymous_fun_173_))(par_map_input_28_),(fun_comp(anonymous_fun_174_,anonymous_fun_175_))(par_map_input_28_))
              def anonymous_fun_176_(par_map_input_29_):
                """
                par_map_input_29_: ((Vec Double),Bool)
                """
                def anonymous_fun_177_(row_29_):
                  """
                  row_29_: ((Vec Double),Bool)
                  """
                  if (row_29_[0])[4] == 5.0:
                    cond_result_39_ = 1.0
                  else:
                    cond_result_39_ = 0.0
                  return cond_result_39_
                def anonymous_fun_178_(dbrow_39_):
                  """
                  dbrow_39_: ((Vec Double),Bool)
                  """
                  return dbrow_39_
                def anonymous_fun_179_(row_30_):
                  """
                  row_30_: ((Vec Double),Bool)
                  """
                  if row_30_[1] and (row_30_[0])[4] == 5.0:
                    cond_result_40_ = 1.0
                  else:
                    cond_result_40_ = 0.0
                  return cond_result_40_
                def anonymous_fun_180_(dbrow_40_):
                  """
                  dbrow_40_: ((Vec Double),Bool)
                  """
                  return dbrow_40_
                return ((fun_comp(anonymous_fun_177_,anonymous_fun_178_))(par_map_input_29_),(fun_comp(anonymous_fun_179_,anonymous_fun_180_))(par_map_input_29_))
              return (anonymous_fun_171_(par_map_input_30_),anonymous_fun_176_(par_map_input_30_))
            return (anonymous_fun_165_(par_map_input_31_),anonymous_fun_170_(par_map_input_31_))
          return (anonymous_fun_159_(par_map_input_32_),anonymous_fun_164_(par_map_input_32_))
        return (anonymous_fun_153_(par_map_input_33_),anonymous_fun_158_(par_map_input_33_))
      return (anonymous_fun_147_(par_map_input_34_),anonymous_fun_152_(par_map_input_34_))
    return anonymous_fun_146_
  def anonymous_fun_181_(empty_closure_11_):
    """
    empty_closure_11_: ()
    """
    def anonymous_fun_182_(par_release_input_34_):
      """
      par_release_input_34_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double))))))
      """
      def anonymous_fun_183_(par_release_input_24_):
        """
        par_release_input_24_: (Double,Double)
        """
        def anonymous_fun_184_(orange_input_29_):
          """
          orange_input_29_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_29_)
        def anonymous_fun_185_(orange_input_30_):
          """
          orange_input_30_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_30_)
        return (anonymous_fun_184_(par_release_input_24_[0]),anonymous_fun_185_(par_release_input_24_[1]))
      def anonymous_fun_186_(par_release_input_33_):
        """
        par_release_input_33_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))
        """
        def anonymous_fun_187_(par_release_input_25_):
          """
          par_release_input_25_: (Double,Double)
          """
          def anonymous_fun_188_(orange_input_31_):
            """
            orange_input_31_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_31_)
          def anonymous_fun_189_(orange_input_32_):
            """
            orange_input_32_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_32_)
          return (anonymous_fun_188_(par_release_input_25_[0]),anonymous_fun_189_(par_release_input_25_[1]))
        def anonymous_fun_190_(par_release_input_32_):
          """
          par_release_input_32_: ((Double,Double),((Double,Double),((Double,Double),(Double,Double))))
          """
          def anonymous_fun_191_(par_release_input_26_):
            """
            par_release_input_26_: (Double,Double)
            """
            def anonymous_fun_192_(orange_input_33_):
              """
              orange_input_33_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_33_)
            def anonymous_fun_193_(orange_input_34_):
              """
              orange_input_34_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_34_)
            return (anonymous_fun_192_(par_release_input_26_[0]),anonymous_fun_193_(par_release_input_26_[1]))
          def anonymous_fun_194_(par_release_input_31_):
            """
            par_release_input_31_: ((Double,Double),((Double,Double),(Double,Double)))
            """
            def anonymous_fun_195_(par_release_input_27_):
              """
              par_release_input_27_: (Double,Double)
              """
              def anonymous_fun_196_(orange_input_35_):
                """
                orange_input_35_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_35_)
              def anonymous_fun_197_(orange_input_36_):
                """
                orange_input_36_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_36_)
              return (anonymous_fun_196_(par_release_input_27_[0]),anonymous_fun_197_(par_release_input_27_[1]))
            def anonymous_fun_198_(par_release_input_30_):
              """
              par_release_input_30_: ((Double,Double),(Double,Double))
              """
              def anonymous_fun_199_(par_release_input_28_):
                """
                par_release_input_28_: (Double,Double)
                """
                def anonymous_fun_200_(orange_input_37_):
                  """
                  orange_input_37_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_37_)
                def anonymous_fun_201_(orange_input_38_):
                  """
                  orange_input_38_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_38_)
                return (anonymous_fun_200_(par_release_input_28_[0]),anonymous_fun_201_(par_release_input_28_[1]))
              def anonymous_fun_202_(par_release_input_29_):
                """
                par_release_input_29_: (Double,Double)
                """
                def anonymous_fun_203_(orange_input_39_):
                  """
                  orange_input_39_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_39_)
                def anonymous_fun_204_(orange_input_40_):
                  """
                  orange_input_40_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_40_)
                return (anonymous_fun_203_(par_release_input_29_[0]),anonymous_fun_204_(par_release_input_29_[1]))
              return (anonymous_fun_199_(par_release_input_30_[0]),anonymous_fun_202_(par_release_input_30_[1]))
            return (anonymous_fun_195_(par_release_input_31_[0]),anonymous_fun_198_(par_release_input_31_[1]))
          return (anonymous_fun_191_(par_release_input_32_[0]),anonymous_fun_194_(par_release_input_32_[1]))
        return (anonymous_fun_187_(par_release_input_33_[0]),anonymous_fun_190_(par_release_input_33_[1]))
      return (anonymous_fun_183_(par_release_input_34_[0]),anonymous_fun_186_(par_release_input_34_[1]))
    return anonymous_fun_182_
  fused_vector_tup_4_ = bmcs(12,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_145_,(),anonymous_fun_181_)
  fused_samples_4_ = [fused_vector_tup_4_[0],(fused_vector_tup_4_[1])[0],((fused_vector_tup_4_[1])[1])[0],(((fused_vector_tup_4_[1])[1])[1])[0],((((fused_vector_tup_4_[1])[1])[1])[1])[0],((((fused_vector_tup_4_[1])[1])[1])[1])[1]]
  def anonymous_fun_205_(empty_closure_12_):
    """
    empty_closure_12_: ()
    """
    def anonymous_fun_206_(par_map_input_47_):
      """
      par_map_input_47_: ((Vec Double),Bool)
      """
      def anonymous_fun_207_(par_map_input_35_):
        """
        par_map_input_35_: ((Vec Double),Bool)
        """
        def anonymous_fun_208_(row_53_):
          """
          row_53_: ((Vec Double),Bool)
          """
          if (row_53_[0])[5] == 0.0:
            cond_result_41_ = 1.0
          else:
            cond_result_41_ = 0.0
          return cond_result_41_
        def anonymous_fun_209_(dbrow_41_):
          """
          dbrow_41_: ((Vec Double),Bool)
          """
          return dbrow_41_
        def anonymous_fun_210_(row_54_):
          """
          row_54_: ((Vec Double),Bool)
          """
          if row_54_[1] and (row_54_[0])[5] == 0.0:
            cond_result_42_ = 1.0
          else:
            cond_result_42_ = 0.0
          return cond_result_42_
        def anonymous_fun_211_(dbrow_42_):
          """
          dbrow_42_: ((Vec Double),Bool)
          """
          return dbrow_42_
        return ((fun_comp(anonymous_fun_208_,anonymous_fun_209_))(par_map_input_35_),(fun_comp(anonymous_fun_210_,anonymous_fun_211_))(par_map_input_35_))
      def anonymous_fun_212_(par_map_input_46_):
        """
        par_map_input_46_: ((Vec Double),Bool)
        """
        def anonymous_fun_213_(par_map_input_36_):
          """
          par_map_input_36_: ((Vec Double),Bool)
          """
          def anonymous_fun_214_(row_51_):
            """
            row_51_: ((Vec Double),Bool)
            """
            if (row_51_[0])[5] == 1.0:
              cond_result_43_ = 1.0
            else:
              cond_result_43_ = 0.0
            return cond_result_43_
          def anonymous_fun_215_(dbrow_43_):
            """
            dbrow_43_: ((Vec Double),Bool)
            """
            return dbrow_43_
          def anonymous_fun_216_(row_52_):
            """
            row_52_: ((Vec Double),Bool)
            """
            if row_52_[1] and (row_52_[0])[5] == 1.0:
              cond_result_44_ = 1.0
            else:
              cond_result_44_ = 0.0
            return cond_result_44_
          def anonymous_fun_217_(dbrow_44_):
            """
            dbrow_44_: ((Vec Double),Bool)
            """
            return dbrow_44_
          return ((fun_comp(anonymous_fun_214_,anonymous_fun_215_))(par_map_input_36_),(fun_comp(anonymous_fun_216_,anonymous_fun_217_))(par_map_input_36_))
        def anonymous_fun_218_(par_map_input_45_):
          """
          par_map_input_45_: ((Vec Double),Bool)
          """
          def anonymous_fun_219_(par_map_input_37_):
            """
            par_map_input_37_: ((Vec Double),Bool)
            """
            def anonymous_fun_220_(row_49_):
              """
              row_49_: ((Vec Double),Bool)
              """
              if (row_49_[0])[5] == 2.0:
                cond_result_45_ = 1.0
              else:
                cond_result_45_ = 0.0
              return cond_result_45_
            def anonymous_fun_221_(dbrow_45_):
              """
              dbrow_45_: ((Vec Double),Bool)
              """
              return dbrow_45_
            def anonymous_fun_222_(row_50_):
              """
              row_50_: ((Vec Double),Bool)
              """
              if row_50_[1] and (row_50_[0])[5] == 2.0:
                cond_result_46_ = 1.0
              else:
                cond_result_46_ = 0.0
              return cond_result_46_
            def anonymous_fun_223_(dbrow_46_):
              """
              dbrow_46_: ((Vec Double),Bool)
              """
              return dbrow_46_
            return ((fun_comp(anonymous_fun_220_,anonymous_fun_221_))(par_map_input_37_),(fun_comp(anonymous_fun_222_,anonymous_fun_223_))(par_map_input_37_))
          def anonymous_fun_224_(par_map_input_44_):
            """
            par_map_input_44_: ((Vec Double),Bool)
            """
            def anonymous_fun_225_(par_map_input_38_):
              """
              par_map_input_38_: ((Vec Double),Bool)
              """
              def anonymous_fun_226_(row_47_):
                """
                row_47_: ((Vec Double),Bool)
                """
                if (row_47_[0])[5] == 3.0:
                  cond_result_47_ = 1.0
                else:
                  cond_result_47_ = 0.0
                return cond_result_47_
              def anonymous_fun_227_(dbrow_47_):
                """
                dbrow_47_: ((Vec Double),Bool)
                """
                return dbrow_47_
              def anonymous_fun_228_(row_48_):
                """
                row_48_: ((Vec Double),Bool)
                """
                if row_48_[1] and (row_48_[0])[5] == 3.0:
                  cond_result_48_ = 1.0
                else:
                  cond_result_48_ = 0.0
                return cond_result_48_
              def anonymous_fun_229_(dbrow_48_):
                """
                dbrow_48_: ((Vec Double),Bool)
                """
                return dbrow_48_
              return ((fun_comp(anonymous_fun_226_,anonymous_fun_227_))(par_map_input_38_),(fun_comp(anonymous_fun_228_,anonymous_fun_229_))(par_map_input_38_))
            def anonymous_fun_230_(par_map_input_43_):
              """
              par_map_input_43_: ((Vec Double),Bool)
              """
              def anonymous_fun_231_(par_map_input_39_):
                """
                par_map_input_39_: ((Vec Double),Bool)
                """
                def anonymous_fun_232_(row_45_):
                  """
                  row_45_: ((Vec Double),Bool)
                  """
                  if (row_45_[0])[5] == 4.0:
                    cond_result_49_ = 1.0
                  else:
                    cond_result_49_ = 0.0
                  return cond_result_49_
                def anonymous_fun_233_(dbrow_49_):
                  """
                  dbrow_49_: ((Vec Double),Bool)
                  """
                  return dbrow_49_
                def anonymous_fun_234_(row_46_):
                  """
                  row_46_: ((Vec Double),Bool)
                  """
                  if row_46_[1] and (row_46_[0])[5] == 4.0:
                    cond_result_50_ = 1.0
                  else:
                    cond_result_50_ = 0.0
                  return cond_result_50_
                def anonymous_fun_235_(dbrow_50_):
                  """
                  dbrow_50_: ((Vec Double),Bool)
                  """
                  return dbrow_50_
                return ((fun_comp(anonymous_fun_232_,anonymous_fun_233_))(par_map_input_39_),(fun_comp(anonymous_fun_234_,anonymous_fun_235_))(par_map_input_39_))
              def anonymous_fun_236_(par_map_input_42_):
                """
                par_map_input_42_: ((Vec Double),Bool)
                """
                def anonymous_fun_237_(par_map_input_40_):
                  """
                  par_map_input_40_: ((Vec Double),Bool)
                  """
                  def anonymous_fun_238_(row_43_):
                    """
                    row_43_: ((Vec Double),Bool)
                    """
                    if (row_43_[0])[5] == 5.0:
                      cond_result_51_ = 1.0
                    else:
                      cond_result_51_ = 0.0
                    return cond_result_51_
                  def anonymous_fun_239_(dbrow_51_):
                    """
                    dbrow_51_: ((Vec Double),Bool)
                    """
                    return dbrow_51_
                  def anonymous_fun_240_(row_44_):
                    """
                    row_44_: ((Vec Double),Bool)
                    """
                    if row_44_[1] and (row_44_[0])[5] == 5.0:
                      cond_result_52_ = 1.0
                    else:
                      cond_result_52_ = 0.0
                    return cond_result_52_
                  def anonymous_fun_241_(dbrow_52_):
                    """
                    dbrow_52_: ((Vec Double),Bool)
                    """
                    return dbrow_52_
                  return ((fun_comp(anonymous_fun_238_,anonymous_fun_239_))(par_map_input_40_),(fun_comp(anonymous_fun_240_,anonymous_fun_241_))(par_map_input_40_))
                def anonymous_fun_242_(par_map_input_41_):
                  """
                  par_map_input_41_: ((Vec Double),Bool)
                  """
                  def anonymous_fun_243_(row_41_):
                    """
                    row_41_: ((Vec Double),Bool)
                    """
                    if (row_41_[0])[5] == 6.0:
                      cond_result_53_ = 1.0
                    else:
                      cond_result_53_ = 0.0
                    return cond_result_53_
                  def anonymous_fun_244_(dbrow_53_):
                    """
                    dbrow_53_: ((Vec Double),Bool)
                    """
                    return dbrow_53_
                  def anonymous_fun_245_(row_42_):
                    """
                    row_42_: ((Vec Double),Bool)
                    """
                    if row_42_[1] and (row_42_[0])[5] == 6.0:
                      cond_result_54_ = 1.0
                    else:
                      cond_result_54_ = 0.0
                    return cond_result_54_
                  def anonymous_fun_246_(dbrow_54_):
                    """
                    dbrow_54_: ((Vec Double),Bool)
                    """
                    return dbrow_54_
                  return ((fun_comp(anonymous_fun_243_,anonymous_fun_244_))(par_map_input_41_),(fun_comp(anonymous_fun_245_,anonymous_fun_246_))(par_map_input_41_))
                return (anonymous_fun_237_(par_map_input_42_),anonymous_fun_242_(par_map_input_42_))
              return (anonymous_fun_231_(par_map_input_43_),anonymous_fun_236_(par_map_input_43_))
            return (anonymous_fun_225_(par_map_input_44_),anonymous_fun_230_(par_map_input_44_))
          return (anonymous_fun_219_(par_map_input_45_),anonymous_fun_224_(par_map_input_45_))
        return (anonymous_fun_213_(par_map_input_46_),anonymous_fun_218_(par_map_input_46_))
      return (anonymous_fun_207_(par_map_input_47_),anonymous_fun_212_(par_map_input_47_))
    return anonymous_fun_206_
  def anonymous_fun_247_(empty_closure_13_):
    """
    empty_closure_13_: ()
    """
    def anonymous_fun_248_(par_release_input_47_):
      """
      par_release_input_47_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))))
      """
      def anonymous_fun_249_(par_release_input_35_):
        """
        par_release_input_35_: (Double,Double)
        """
        def anonymous_fun_250_(orange_input_41_):
          """
          orange_input_41_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_41_)
        def anonymous_fun_251_(orange_input_42_):
          """
          orange_input_42_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_42_)
        return (anonymous_fun_250_(par_release_input_35_[0]),anonymous_fun_251_(par_release_input_35_[1]))
      def anonymous_fun_252_(par_release_input_46_):
        """
        par_release_input_46_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double))))))
        """
        def anonymous_fun_253_(par_release_input_36_):
          """
          par_release_input_36_: (Double,Double)
          """
          def anonymous_fun_254_(orange_input_43_):
            """
            orange_input_43_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_43_)
          def anonymous_fun_255_(orange_input_44_):
            """
            orange_input_44_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_44_)
          return (anonymous_fun_254_(par_release_input_36_[0]),anonymous_fun_255_(par_release_input_36_[1]))
        def anonymous_fun_256_(par_release_input_45_):
          """
          par_release_input_45_: ((Double,Double),((Double,Double),((Double,Double),((Double,Double),(Double,Double)))))
          """
          def anonymous_fun_257_(par_release_input_37_):
            """
            par_release_input_37_: (Double,Double)
            """
            def anonymous_fun_258_(orange_input_45_):
              """
              orange_input_45_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_45_)
            def anonymous_fun_259_(orange_input_46_):
              """
              orange_input_46_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_46_)
            return (anonymous_fun_258_(par_release_input_37_[0]),anonymous_fun_259_(par_release_input_37_[1]))
          def anonymous_fun_260_(par_release_input_44_):
            """
            par_release_input_44_: ((Double,Double),((Double,Double),((Double,Double),(Double,Double))))
            """
            def anonymous_fun_261_(par_release_input_38_):
              """
              par_release_input_38_: (Double,Double)
              """
              def anonymous_fun_262_(orange_input_47_):
                """
                orange_input_47_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_47_)
              def anonymous_fun_263_(orange_input_48_):
                """
                orange_input_48_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_48_)
              return (anonymous_fun_262_(par_release_input_38_[0]),anonymous_fun_263_(par_release_input_38_[1]))
            def anonymous_fun_264_(par_release_input_43_):
              """
              par_release_input_43_: ((Double,Double),((Double,Double),(Double,Double)))
              """
              def anonymous_fun_265_(par_release_input_39_):
                """
                par_release_input_39_: (Double,Double)
                """
                def anonymous_fun_266_(orange_input_49_):
                  """
                  orange_input_49_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_49_)
                def anonymous_fun_267_(orange_input_50_):
                  """
                  orange_input_50_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_50_)
                return (anonymous_fun_266_(par_release_input_39_[0]),anonymous_fun_267_(par_release_input_39_[1]))
              def anonymous_fun_268_(par_release_input_42_):
                """
                par_release_input_42_: ((Double,Double),(Double,Double))
                """
                def anonymous_fun_269_(par_release_input_40_):
                  """
                  par_release_input_40_: (Double,Double)
                  """
                  def anonymous_fun_270_(orange_input_51_):
                    """
                    orange_input_51_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_51_)
                  def anonymous_fun_271_(orange_input_52_):
                    """
                    orange_input_52_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_52_)
                  return (anonymous_fun_270_(par_release_input_40_[0]),anonymous_fun_271_(par_release_input_40_[1]))
                def anonymous_fun_272_(par_release_input_41_):
                  """
                  par_release_input_41_: (Double,Double)
                  """
                  def anonymous_fun_273_(orange_input_53_):
                    """
                    orange_input_53_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_53_)
                  def anonymous_fun_274_(orange_input_54_):
                    """
                    orange_input_54_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_54_)
                  return (anonymous_fun_273_(par_release_input_41_[0]),anonymous_fun_274_(par_release_input_41_[1]))
                return (anonymous_fun_269_(par_release_input_42_[0]),anonymous_fun_272_(par_release_input_42_[1]))
              return (anonymous_fun_265_(par_release_input_43_[0]),anonymous_fun_268_(par_release_input_43_[1]))
            return (anonymous_fun_261_(par_release_input_44_[0]),anonymous_fun_264_(par_release_input_44_[1]))
          return (anonymous_fun_257_(par_release_input_45_[0]),anonymous_fun_260_(par_release_input_45_[1]))
        return (anonymous_fun_253_(par_release_input_46_[0]),anonymous_fun_256_(par_release_input_46_[1]))
      return (anonymous_fun_249_(par_release_input_47_[0]),anonymous_fun_252_(par_release_input_47_[1]))
    return anonymous_fun_248_
  fused_vector_tup_5_ = bmcs(14,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_205_,(),anonymous_fun_247_)
  fused_samples_5_ = [fused_vector_tup_5_[0],(fused_vector_tup_5_[1])[0],((fused_vector_tup_5_[1])[1])[0],(((fused_vector_tup_5_[1])[1])[1])[0],((((fused_vector_tup_5_[1])[1])[1])[1])[0],(((((fused_vector_tup_5_[1])[1])[1])[1])[1])[0],(((((fused_vector_tup_5_[1])[1])[1])[1])[1])[1]]
  def anonymous_fun_275_(curr_acc_0_):
    """
    curr_acc_0_: (((Vec Double),Int),Double)
    """
    return len((curr_acc_0_[0])[0]) > 0
  def anonymous_fun_276_(curr_acc_1_):
    """
    curr_acc_1_: (((Vec Double),Int),Double)
    """
    if ((curr_acc_1_[0])[0])[len((curr_acc_1_[0])[0]) - 1] > curr_acc_1_[1]:
      cond_result_55_ = ((((curr_acc_1_[0])[0])[0:len((curr_acc_1_[0])[0]) - 1],len((curr_acc_1_[0])[0]) - 1),((curr_acc_1_[0])[0])[len((curr_acc_1_[0])[0]) - 1])
    else:
      cond_result_55_ = ((((curr_acc_1_[0])[0])[0:len((curr_acc_1_[0])[0]) - 1],(curr_acc_1_[0])[1]),curr_acc_1_[1])
    return cond_result_55_
  loop_acc_0_ = (([(0.0 + (fused_samples_0_[0])[1] * math.log((fused_samples_0_[0])[1] / (fused_samples_0_[0])[0])) + (fused_samples_0_[1])[1] * math.log((fused_samples_0_[1])[1] / (fused_samples_0_[1])[0]),((0.0 + (fused_samples_1_[0])[1] * math.log((fused_samples_1_[0])[1] / (fused_samples_1_[0])[0])) + (fused_samples_1_[1])[1] * math.log((fused_samples_1_[1])[1] / (fused_samples_1_[1])[0])) + (fused_samples_1_[2])[1] * math.log((fused_samples_1_[2])[1] / (fused_samples_1_[2])[0]),(((0.0 + (fused_samples_2_[0])[1] * math.log((fused_samples_2_[0])[1] / (fused_samples_2_[0])[0])) + (fused_samples_2_[1])[1] * math.log((fused_samples_2_[1])[1] / (fused_samples_2_[1])[0])) + (fused_samples_2_[2])[1] * math.log((fused_samples_2_[2])[1] / (fused_samples_2_[2])[0])) + (fused_samples_2_[3])[1] * math.log((fused_samples_2_[3])[1] / (fused_samples_2_[3])[0]),((((0.0 + (fused_samples_3_[0])[1] * math.log((fused_samples_3_[0])[1] / (fused_samples_3_[0])[0])) + (fused_samples_3_[1])[1] * math.log((fused_samples_3_[1])[1] / (fused_samples_3_[1])[0])) + (fused_samples_3_[2])[1] * math.log((fused_samples_3_[2])[1] / (fused_samples_3_[2])[0])) + (fused_samples_3_[3])[1] * math.log((fused_samples_3_[3])[1] / (fused_samples_3_[3])[0])) + (fused_samples_3_[4])[1] * math.log((fused_samples_3_[4])[1] / (fused_samples_3_[4])[0]),(((((0.0 + (fused_samples_4_[0])[1] * math.log((fused_samples_4_[0])[1] / (fused_samples_4_[0])[0])) + (fused_samples_4_[1])[1] * math.log((fused_samples_4_[1])[1] / (fused_samples_4_[1])[0])) + (fused_samples_4_[2])[1] * math.log((fused_samples_4_[2])[1] / (fused_samples_4_[2])[0])) + (fused_samples_4_[3])[1] * math.log((fused_samples_4_[3])[1] / (fused_samples_4_[3])[0])) + (fused_samples_4_[4])[1] * math.log((fused_samples_4_[4])[1] / (fused_samples_4_[4])[0])) + (fused_samples_4_[5])[1] * math.log((fused_samples_4_[5])[1] / (fused_samples_4_[5])[0]),((((((0.0 + (fused_samples_5_[0])[1] * math.log((fused_samples_5_[0])[1] / (fused_samples_5_[0])[0])) + (fused_samples_5_[1])[1] * math.log((fused_samples_5_[1])[1] / (fused_samples_5_[1])[0])) + (fused_samples_5_[2])[1] * math.log((fused_samples_5_[2])[1] / (fused_samples_5_[2])[0])) + (fused_samples_5_[3])[1] * math.log((fused_samples_5_[3])[1] / (fused_samples_5_[3])[0])) + (fused_samples_5_[4])[1] * math.log((fused_samples_5_[4])[1] / (fused_samples_5_[4])[0])) + (fused_samples_5_[5])[1] * math.log((fused_samples_5_[5])[1] / (fused_samples_5_[5])[0])) + (fused_samples_5_[6])[1] * math.log((fused_samples_5_[6])[1] / (fused_samples_5_[6])[0])],0 - 1),0.0 - 99999.0)
  while anonymous_fun_275_(loop_acc_0_):
    loop_acc_0_ = anonymous_fun_276_(loop_acc_0_)
  cond_result_0_ = (loop_acc_0_[0])[1]
cond_result_0_