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
  def anonymous_fun_1_(par_map_input_28_):
    """
    par_map_input_28_: Double
    """
    def anonymous_fun_2_(row_29_):
      """
      row_29_: Double
      """
      if 5.0 <= row_29_ and row_29_ < 10.0:
        cond_result_0_ = 1.0
      else:
        cond_result_0_ = 0.0
      return cond_result_0_
    def anonymous_fun_3_(dbrow_0_):
      """
      dbrow_0_: Double
      """
      return dbrow_0_
    def anonymous_fun_4_(par_map_input_27_):
      """
      par_map_input_27_: Double
      """
      def anonymous_fun_5_(row_28_):
        """
        row_28_: Double
        """
        if 0.0 <= row_28_ and row_28_ < 5.0:
          cond_result_1_ = 1.0
        else:
          cond_result_1_ = 0.0
        return cond_result_1_
      def anonymous_fun_6_(dbrow_1_):
        """
        dbrow_1_: Double
        """
        return dbrow_1_
      def anonymous_fun_7_(par_map_input_26_):
        """
        par_map_input_26_: Double
        """
        def anonymous_fun_8_(row_27_):
          """
          row_27_: Double
          """
          if 7.5 <= row_27_ and row_27_ < 10.0:
            cond_result_2_ = 1.0
          else:
            cond_result_2_ = 0.0
          return cond_result_2_
        def anonymous_fun_9_(dbrow_2_):
          """
          dbrow_2_: Double
          """
          return dbrow_2_
        def anonymous_fun_10_(par_map_input_25_):
          """
          par_map_input_25_: Double
          """
          def anonymous_fun_11_(row_26_):
            """
            row_26_: Double
            """
            if 5.0 <= row_26_ and row_26_ < 7.5:
              cond_result_3_ = 1.0
            else:
              cond_result_3_ = 0.0
            return cond_result_3_
          def anonymous_fun_12_(dbrow_3_):
            """
            dbrow_3_: Double
            """
            return dbrow_3_
          def anonymous_fun_13_(par_map_input_24_):
            """
            par_map_input_24_: Double
            """
            def anonymous_fun_14_(row_25_):
              """
              row_25_: Double
              """
              if 8.75 <= row_25_ and row_25_ < 10.0:
                cond_result_4_ = 1.0
              else:
                cond_result_4_ = 0.0
              return cond_result_4_
            def anonymous_fun_15_(dbrow_4_):
              """
              dbrow_4_: Double
              """
              return dbrow_4_
            def anonymous_fun_16_(par_map_input_23_):
              """
              par_map_input_23_: Double
              """
              def anonymous_fun_17_(row_24_):
                """
                row_24_: Double
                """
                if 7.5 <= row_24_ and row_24_ < 8.75:
                  cond_result_5_ = 1.0
                else:
                  cond_result_5_ = 0.0
                return cond_result_5_
              def anonymous_fun_18_(dbrow_5_):
                """
                dbrow_5_: Double
                """
                return dbrow_5_
              def anonymous_fun_19_(par_map_input_22_):
                """
                par_map_input_22_: Double
                """
                def anonymous_fun_20_(row_23_):
                  """
                  row_23_: Double
                  """
                  if 9.375 <= row_23_ and row_23_ < 10.0:
                    cond_result_6_ = 1.0
                  else:
                    cond_result_6_ = 0.0
                  return cond_result_6_
                def anonymous_fun_21_(dbrow_6_):
                  """
                  dbrow_6_: Double
                  """
                  return dbrow_6_
                def anonymous_fun_22_(par_map_input_21_):
                  """
                  par_map_input_21_: Double
                  """
                  def anonymous_fun_23_(row_22_):
                    """
                    row_22_: Double
                    """
                    if 8.75 <= row_22_ and row_22_ < 9.375:
                      cond_result_7_ = 1.0
                    else:
                      cond_result_7_ = 0.0
                    return cond_result_7_
                  def anonymous_fun_24_(dbrow_7_):
                    """
                    dbrow_7_: Double
                    """
                    return dbrow_7_
                  def anonymous_fun_25_(par_map_input_20_):
                    """
                    par_map_input_20_: Double
                    """
                    def anonymous_fun_26_(row_21_):
                      """
                      row_21_: Double
                      """
                      if 8.125 <= row_21_ and row_21_ < 8.75:
                        cond_result_8_ = 1.0
                      else:
                        cond_result_8_ = 0.0
                      return cond_result_8_
                    def anonymous_fun_27_(dbrow_8_):
                      """
                      dbrow_8_: Double
                      """
                      return dbrow_8_
                    def anonymous_fun_28_(par_map_input_19_):
                      """
                      par_map_input_19_: Double
                      """
                      def anonymous_fun_29_(row_20_):
                        """
                        row_20_: Double
                        """
                        if 7.5 <= row_20_ and row_20_ < 8.125:
                          cond_result_9_ = 1.0
                        else:
                          cond_result_9_ = 0.0
                        return cond_result_9_
                      def anonymous_fun_30_(dbrow_9_):
                        """
                        dbrow_9_: Double
                        """
                        return dbrow_9_
                      def anonymous_fun_31_(par_map_input_18_):
                        """
                        par_map_input_18_: Double
                        """
                        def anonymous_fun_32_(row_19_):
                          """
                          row_19_: Double
                          """
                          if 6.25 <= row_19_ and row_19_ < 7.5:
                            cond_result_10_ = 1.0
                          else:
                            cond_result_10_ = 0.0
                          return cond_result_10_
                        def anonymous_fun_33_(dbrow_10_):
                          """
                          dbrow_10_: Double
                          """
                          return dbrow_10_
                        def anonymous_fun_34_(par_map_input_17_):
                          """
                          par_map_input_17_: Double
                          """
                          def anonymous_fun_35_(row_18_):
                            """
                            row_18_: Double
                            """
                            if 5.0 <= row_18_ and row_18_ < 6.25:
                              cond_result_11_ = 1.0
                            else:
                              cond_result_11_ = 0.0
                            return cond_result_11_
                          def anonymous_fun_36_(dbrow_11_):
                            """
                            dbrow_11_: Double
                            """
                            return dbrow_11_
                          def anonymous_fun_37_(par_map_input_16_):
                            """
                            par_map_input_16_: Double
                            """
                            def anonymous_fun_38_(row_17_):
                              """
                              row_17_: Double
                              """
                              if 6.875 <= row_17_ and row_17_ < 7.5:
                                cond_result_12_ = 1.0
                              else:
                                cond_result_12_ = 0.0
                              return cond_result_12_
                            def anonymous_fun_39_(dbrow_12_):
                              """
                              dbrow_12_: Double
                              """
                              return dbrow_12_
                            def anonymous_fun_40_(par_map_input_15_):
                              """
                              par_map_input_15_: Double
                              """
                              def anonymous_fun_41_(row_16_):
                                """
                                row_16_: Double
                                """
                                if 6.25 <= row_16_ and row_16_ < 6.875:
                                  cond_result_13_ = 1.0
                                else:
                                  cond_result_13_ = 0.0
                                return cond_result_13_
                              def anonymous_fun_42_(dbrow_13_):
                                """
                                dbrow_13_: Double
                                """
                                return dbrow_13_
                              def anonymous_fun_43_(par_map_input_14_):
                                """
                                par_map_input_14_: Double
                                """
                                def anonymous_fun_44_(row_15_):
                                  """
                                  row_15_: Double
                                  """
                                  if 5.625 <= row_15_ and row_15_ < 6.25:
                                    cond_result_14_ = 1.0
                                  else:
                                    cond_result_14_ = 0.0
                                  return cond_result_14_
                                def anonymous_fun_45_(dbrow_14_):
                                  """
                                  dbrow_14_: Double
                                  """
                                  return dbrow_14_
                                def anonymous_fun_46_(par_map_input_13_):
                                  """
                                  par_map_input_13_: Double
                                  """
                                  def anonymous_fun_47_(row_14_):
                                    """
                                    row_14_: Double
                                    """
                                    if 5.0 <= row_14_ and row_14_ < 5.625:
                                      cond_result_15_ = 1.0
                                    else:
                                      cond_result_15_ = 0.0
                                    return cond_result_15_
                                  def anonymous_fun_48_(dbrow_15_):
                                    """
                                    dbrow_15_: Double
                                    """
                                    return dbrow_15_
                                  def anonymous_fun_49_(par_map_input_12_):
                                    """
                                    par_map_input_12_: Double
                                    """
                                    def anonymous_fun_50_(row_13_):
                                      """
                                      row_13_: Double
                                      """
                                      if 2.5 <= row_13_ and row_13_ < 5.0:
                                        cond_result_16_ = 1.0
                                      else:
                                        cond_result_16_ = 0.0
                                      return cond_result_16_
                                    def anonymous_fun_51_(dbrow_16_):
                                      """
                                      dbrow_16_: Double
                                      """
                                      return dbrow_16_
                                    def anonymous_fun_52_(par_map_input_11_):
                                      """
                                      par_map_input_11_: Double
                                      """
                                      def anonymous_fun_53_(row_12_):
                                        """
                                        row_12_: Double
                                        """
                                        if 0.0 <= row_12_ and row_12_ < 2.5:
                                          cond_result_17_ = 1.0
                                        else:
                                          cond_result_17_ = 0.0
                                        return cond_result_17_
                                      def anonymous_fun_54_(dbrow_17_):
                                        """
                                        dbrow_17_: Double
                                        """
                                        return dbrow_17_
                                      def anonymous_fun_55_(par_map_input_10_):
                                        """
                                        par_map_input_10_: Double
                                        """
                                        def anonymous_fun_56_(row_11_):
                                          """
                                          row_11_: Double
                                          """
                                          if 3.75 <= row_11_ and row_11_ < 5.0:
                                            cond_result_18_ = 1.0
                                          else:
                                            cond_result_18_ = 0.0
                                          return cond_result_18_
                                        def anonymous_fun_57_(dbrow_18_):
                                          """
                                          dbrow_18_: Double
                                          """
                                          return dbrow_18_
                                        def anonymous_fun_58_(par_map_input_9_):
                                          """
                                          par_map_input_9_: Double
                                          """
                                          def anonymous_fun_59_(row_10_):
                                            """
                                            row_10_: Double
                                            """
                                            if 2.5 <= row_10_ and row_10_ < 3.75:
                                              cond_result_19_ = 1.0
                                            else:
                                              cond_result_19_ = 0.0
                                            return cond_result_19_
                                          def anonymous_fun_60_(dbrow_19_):
                                            """
                                            dbrow_19_: Double
                                            """
                                            return dbrow_19_
                                          def anonymous_fun_61_(par_map_input_8_):
                                            """
                                            par_map_input_8_: Double
                                            """
                                            def anonymous_fun_62_(row_9_):
                                              """
                                              row_9_: Double
                                              """
                                              if 4.375 <= row_9_ and row_9_ < 5.0:
                                                cond_result_20_ = 1.0
                                              else:
                                                cond_result_20_ = 0.0
                                              return cond_result_20_
                                            def anonymous_fun_63_(dbrow_20_):
                                              """
                                              dbrow_20_: Double
                                              """
                                              return dbrow_20_
                                            def anonymous_fun_64_(par_map_input_7_):
                                              """
                                              par_map_input_7_: Double
                                              """
                                              def anonymous_fun_65_(row_8_):
                                                """
                                                row_8_: Double
                                                """
                                                if 3.75 <= row_8_ and row_8_ < 4.375:
                                                  cond_result_21_ = 1.0
                                                else:
                                                  cond_result_21_ = 0.0
                                                return cond_result_21_
                                              def anonymous_fun_66_(dbrow_21_):
                                                """
                                                dbrow_21_: Double
                                                """
                                                return dbrow_21_
                                              def anonymous_fun_67_(par_map_input_6_):
                                                """
                                                par_map_input_6_: Double
                                                """
                                                def anonymous_fun_68_(row_7_):
                                                  """
                                                  row_7_: Double
                                                  """
                                                  if 3.125 <= row_7_ and row_7_ < 3.75:
                                                    cond_result_22_ = 1.0
                                                  else:
                                                    cond_result_22_ = 0.0
                                                  return cond_result_22_
                                                def anonymous_fun_69_(dbrow_22_):
                                                  """
                                                  dbrow_22_: Double
                                                  """
                                                  return dbrow_22_
                                                def anonymous_fun_70_(par_map_input_5_):
                                                  """
                                                  par_map_input_5_: Double
                                                  """
                                                  def anonymous_fun_71_(row_6_):
                                                    """
                                                    row_6_: Double
                                                    """
                                                    if 2.5 <= row_6_ and row_6_ < 3.125:
                                                      cond_result_23_ = 1.0
                                                    else:
                                                      cond_result_23_ = 0.0
                                                    return cond_result_23_
                                                  def anonymous_fun_72_(dbrow_23_):
                                                    """
                                                    dbrow_23_: Double
                                                    """
                                                    return dbrow_23_
                                                  def anonymous_fun_73_(par_map_input_4_):
                                                    """
                                                    par_map_input_4_: Double
                                                    """
                                                    def anonymous_fun_74_(row_5_):
                                                      """
                                                      row_5_: Double
                                                      """
                                                      if 1.25 <= row_5_ and row_5_ < 2.5:
                                                        cond_result_24_ = 1.0
                                                      else:
                                                        cond_result_24_ = 0.0
                                                      return cond_result_24_
                                                    def anonymous_fun_75_(dbrow_24_):
                                                      """
                                                      dbrow_24_: Double
                                                      """
                                                      return dbrow_24_
                                                    def anonymous_fun_76_(par_map_input_3_):
                                                      """
                                                      par_map_input_3_: Double
                                                      """
                                                      def anonymous_fun_77_(row_4_):
                                                        """
                                                        row_4_: Double
                                                        """
                                                        if 0.0 <= row_4_ and row_4_ < 1.25:
                                                          cond_result_25_ = 1.0
                                                        else:
                                                          cond_result_25_ = 0.0
                                                        return cond_result_25_
                                                      def anonymous_fun_78_(dbrow_25_):
                                                        """
                                                        dbrow_25_: Double
                                                        """
                                                        return dbrow_25_
                                                      def anonymous_fun_79_(par_map_input_2_):
                                                        """
                                                        par_map_input_2_: Double
                                                        """
                                                        def anonymous_fun_80_(row_3_):
                                                          """
                                                          row_3_: Double
                                                          """
                                                          if 1.875 <= row_3_ and row_3_ < 2.5:
                                                            cond_result_26_ = 1.0
                                                          else:
                                                            cond_result_26_ = 0.0
                                                          return cond_result_26_
                                                        def anonymous_fun_81_(dbrow_26_):
                                                          """
                                                          dbrow_26_: Double
                                                          """
                                                          return dbrow_26_
                                                        def anonymous_fun_82_(par_map_input_1_):
                                                          """
                                                          par_map_input_1_: Double
                                                          """
                                                          def anonymous_fun_83_(row_2_):
                                                            """
                                                            row_2_: Double
                                                            """
                                                            if 1.25 <= row_2_ and row_2_ < 1.875:
                                                              cond_result_27_ = 1.0
                                                            else:
                                                              cond_result_27_ = 0.0
                                                            return cond_result_27_
                                                          def anonymous_fun_84_(dbrow_27_):
                                                            """
                                                            dbrow_27_: Double
                                                            """
                                                            return dbrow_27_
                                                          def anonymous_fun_85_(par_map_input_0_):
                                                            """
                                                            par_map_input_0_: Double
                                                            """
                                                            def anonymous_fun_86_(row_1_):
                                                              """
                                                              row_1_: Double
                                                              """
                                                              if 0.625 <= row_1_ and row_1_ < 1.25:
                                                                cond_result_28_ = 1.0
                                                              else:
                                                                cond_result_28_ = 0.0
                                                              return cond_result_28_
                                                            def anonymous_fun_87_(dbrow_28_):
                                                              """
                                                              dbrow_28_: Double
                                                              """
                                                              return dbrow_28_
                                                            def anonymous_fun_88_(row_0_):
                                                              """
                                                              row_0_: Double
                                                              """
                                                              if 0.0 <= row_0_ and row_0_ < 0.625:
                                                                cond_result_29_ = 1.0
                                                              else:
                                                                cond_result_29_ = 0.0
                                                              return cond_result_29_
                                                            def anonymous_fun_89_(dbrow_29_):
                                                              """
                                                              dbrow_29_: Double
                                                              """
                                                              return dbrow_29_
                                                            return ((fun_comp(anonymous_fun_86_,anonymous_fun_87_))(par_map_input_0_),(fun_comp(anonymous_fun_88_,anonymous_fun_89_))(par_map_input_0_))
                                                          return ((fun_comp(anonymous_fun_83_,anonymous_fun_84_))(par_map_input_1_),anonymous_fun_85_(par_map_input_1_))
                                                        return ((fun_comp(anonymous_fun_80_,anonymous_fun_81_))(par_map_input_2_),anonymous_fun_82_(par_map_input_2_))
                                                      return ((fun_comp(anonymous_fun_77_,anonymous_fun_78_))(par_map_input_3_),anonymous_fun_79_(par_map_input_3_))
                                                    return ((fun_comp(anonymous_fun_74_,anonymous_fun_75_))(par_map_input_4_),anonymous_fun_76_(par_map_input_4_))
                                                  return ((fun_comp(anonymous_fun_71_,anonymous_fun_72_))(par_map_input_5_),anonymous_fun_73_(par_map_input_5_))
                                                return ((fun_comp(anonymous_fun_68_,anonymous_fun_69_))(par_map_input_6_),anonymous_fun_70_(par_map_input_6_))
                                              return ((fun_comp(anonymous_fun_65_,anonymous_fun_66_))(par_map_input_7_),anonymous_fun_67_(par_map_input_7_))
                                            return ((fun_comp(anonymous_fun_62_,anonymous_fun_63_))(par_map_input_8_),anonymous_fun_64_(par_map_input_8_))
                                          return ((fun_comp(anonymous_fun_59_,anonymous_fun_60_))(par_map_input_9_),anonymous_fun_61_(par_map_input_9_))
                                        return ((fun_comp(anonymous_fun_56_,anonymous_fun_57_))(par_map_input_10_),anonymous_fun_58_(par_map_input_10_))
                                      return ((fun_comp(anonymous_fun_53_,anonymous_fun_54_))(par_map_input_11_),anonymous_fun_55_(par_map_input_11_))
                                    return ((fun_comp(anonymous_fun_50_,anonymous_fun_51_))(par_map_input_12_),anonymous_fun_52_(par_map_input_12_))
                                  return ((fun_comp(anonymous_fun_47_,anonymous_fun_48_))(par_map_input_13_),anonymous_fun_49_(par_map_input_13_))
                                return ((fun_comp(anonymous_fun_44_,anonymous_fun_45_))(par_map_input_14_),anonymous_fun_46_(par_map_input_14_))
                              return ((fun_comp(anonymous_fun_41_,anonymous_fun_42_))(par_map_input_15_),anonymous_fun_43_(par_map_input_15_))
                            return ((fun_comp(anonymous_fun_38_,anonymous_fun_39_))(par_map_input_16_),anonymous_fun_40_(par_map_input_16_))
                          return ((fun_comp(anonymous_fun_35_,anonymous_fun_36_))(par_map_input_17_),anonymous_fun_37_(par_map_input_17_))
                        return ((fun_comp(anonymous_fun_32_,anonymous_fun_33_))(par_map_input_18_),anonymous_fun_34_(par_map_input_18_))
                      return ((fun_comp(anonymous_fun_29_,anonymous_fun_30_))(par_map_input_19_),anonymous_fun_31_(par_map_input_19_))
                    return ((fun_comp(anonymous_fun_26_,anonymous_fun_27_))(par_map_input_20_),anonymous_fun_28_(par_map_input_20_))
                  return ((fun_comp(anonymous_fun_23_,anonymous_fun_24_))(par_map_input_21_),anonymous_fun_25_(par_map_input_21_))
                return ((fun_comp(anonymous_fun_20_,anonymous_fun_21_))(par_map_input_22_),anonymous_fun_22_(par_map_input_22_))
              return ((fun_comp(anonymous_fun_17_,anonymous_fun_18_))(par_map_input_23_),anonymous_fun_19_(par_map_input_23_))
            return ((fun_comp(anonymous_fun_14_,anonymous_fun_15_))(par_map_input_24_),anonymous_fun_16_(par_map_input_24_))
          return ((fun_comp(anonymous_fun_11_,anonymous_fun_12_))(par_map_input_25_),anonymous_fun_13_(par_map_input_25_))
        return ((fun_comp(anonymous_fun_8_,anonymous_fun_9_))(par_map_input_26_),anonymous_fun_10_(par_map_input_26_))
      return ((fun_comp(anonymous_fun_5_,anonymous_fun_6_))(par_map_input_27_),anonymous_fun_7_(par_map_input_27_))
    return ((fun_comp(anonymous_fun_2_,anonymous_fun_3_))(par_map_input_28_),anonymous_fun_4_(par_map_input_28_))
  return anonymous_fun_1_
def anonymous_fun_90_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_91_(par_release_input_28_):
    """
    par_release_input_28_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))))))))))))
    """
    def anonymous_fun_92_(orange_input_0_):
      """
      orange_input_0_: Double
      """
      return laplace_fx(cfix(1.0),orange_input_0_)
    def anonymous_fun_93_(par_release_input_27_):
      """
      par_release_input_27_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))))))))))))
      """
      def anonymous_fun_94_(orange_input_1_):
        """
        orange_input_1_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_1_)
      def anonymous_fun_95_(par_release_input_26_):
        """
        par_release_input_26_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))))))))))
        """
        def anonymous_fun_96_(orange_input_2_):
          """
          orange_input_2_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_2_)
        def anonymous_fun_97_(par_release_input_25_):
          """
          par_release_input_25_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))))))))))
          """
          def anonymous_fun_98_(orange_input_3_):
            """
            orange_input_3_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_3_)
          def anonymous_fun_99_(par_release_input_24_):
            """
            par_release_input_24_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))))))))
            """
            def anonymous_fun_100_(orange_input_4_):
              """
              orange_input_4_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_4_)
            def anonymous_fun_101_(par_release_input_23_):
              """
              par_release_input_23_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))))))))
              """
              def anonymous_fun_102_(orange_input_5_):
                """
                orange_input_5_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_5_)
              def anonymous_fun_103_(par_release_input_22_):
                """
                par_release_input_22_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))))))
                """
                def anonymous_fun_104_(orange_input_6_):
                  """
                  orange_input_6_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_6_)
                def anonymous_fun_105_(par_release_input_21_):
                  """
                  par_release_input_21_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))))))
                  """
                  def anonymous_fun_106_(orange_input_7_):
                    """
                    orange_input_7_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_7_)
                  def anonymous_fun_107_(par_release_input_20_):
                    """
                    par_release_input_20_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))))
                    """
                    def anonymous_fun_108_(orange_input_8_):
                      """
                      orange_input_8_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_8_)
                    def anonymous_fun_109_(par_release_input_19_):
                      """
                      par_release_input_19_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))))
                      """
                      def anonymous_fun_110_(orange_input_9_):
                        """
                        orange_input_9_: Double
                        """
                        return laplace_fx(cfix(1.0),orange_input_9_)
                      def anonymous_fun_111_(par_release_input_18_):
                        """
                        par_release_input_18_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))))
                        """
                        def anonymous_fun_112_(orange_input_10_):
                          """
                          orange_input_10_: Double
                          """
                          return laplace_fx(cfix(1.0),orange_input_10_)
                        def anonymous_fun_113_(par_release_input_17_):
                          """
                          par_release_input_17_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))))
                          """
                          def anonymous_fun_114_(orange_input_11_):
                            """
                            orange_input_11_: Double
                            """
                            return laplace_fx(cfix(1.0),orange_input_11_)
                          def anonymous_fun_115_(par_release_input_16_):
                            """
                            par_release_input_16_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))))
                            """
                            def anonymous_fun_116_(orange_input_12_):
                              """
                              orange_input_12_: Double
                              """
                              return laplace_fx(cfix(1.0),orange_input_12_)
                            def anonymous_fun_117_(par_release_input_15_):
                              """
                              par_release_input_15_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))))
                              """
                              def anonymous_fun_118_(orange_input_13_):
                                """
                                orange_input_13_: Double
                                """
                                return laplace_fx(cfix(1.0),orange_input_13_)
                              def anonymous_fun_119_(par_release_input_14_):
                                """
                                par_release_input_14_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))))
                                """
                                def anonymous_fun_120_(orange_input_14_):
                                  """
                                  orange_input_14_: Double
                                  """
                                  return laplace_fx(cfix(1.0),orange_input_14_)
                                def anonymous_fun_121_(par_release_input_13_):
                                  """
                                  par_release_input_13_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))))
                                  """
                                  def anonymous_fun_122_(orange_input_15_):
                                    """
                                    orange_input_15_: Double
                                    """
                                    return laplace_fx(cfix(1.0),orange_input_15_)
                                  def anonymous_fun_123_(par_release_input_12_):
                                    """
                                    par_release_input_12_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))))
                                    """
                                    def anonymous_fun_124_(orange_input_16_):
                                      """
                                      orange_input_16_: Double
                                      """
                                      return laplace_fx(cfix(1.0),orange_input_16_)
                                    def anonymous_fun_125_(par_release_input_11_):
                                      """
                                      par_release_input_11_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))))
                                      """
                                      def anonymous_fun_126_(orange_input_17_):
                                        """
                                        orange_input_17_: Double
                                        """
                                        return laplace_fx(cfix(1.0),orange_input_17_)
                                      def anonymous_fun_127_(par_release_input_10_):
                                        """
                                        par_release_input_10_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))))
                                        """
                                        def anonymous_fun_128_(orange_input_18_):
                                          """
                                          orange_input_18_: Double
                                          """
                                          return laplace_fx(cfix(1.0),orange_input_18_)
                                        def anonymous_fun_129_(par_release_input_9_):
                                          """
                                          par_release_input_9_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))))
                                          """
                                          def anonymous_fun_130_(orange_input_19_):
                                            """
                                            orange_input_19_: Double
                                            """
                                            return laplace_fx(cfix(1.0),orange_input_19_)
                                          def anonymous_fun_131_(par_release_input_8_):
                                            """
                                            par_release_input_8_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))
                                            """
                                            def anonymous_fun_132_(orange_input_20_):
                                              """
                                              orange_input_20_: Double
                                              """
                                              return laplace_fx(cfix(1.0),orange_input_20_)
                                            def anonymous_fun_133_(par_release_input_7_):
                                              """
                                              par_release_input_7_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))
                                              """
                                              def anonymous_fun_134_(orange_input_21_):
                                                """
                                                orange_input_21_: Double
                                                """
                                                return laplace_fx(cfix(1.0),orange_input_21_)
                                              def anonymous_fun_135_(par_release_input_6_):
                                                """
                                                par_release_input_6_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))
                                                """
                                                def anonymous_fun_136_(orange_input_22_):
                                                  """
                                                  orange_input_22_: Double
                                                  """
                                                  return laplace_fx(cfix(1.0),orange_input_22_)
                                                def anonymous_fun_137_(par_release_input_5_):
                                                  """
                                                  par_release_input_5_: (Double,(Double,(Double,(Double,(Double,(Double,Double))))))
                                                  """
                                                  def anonymous_fun_138_(orange_input_23_):
                                                    """
                                                    orange_input_23_: Double
                                                    """
                                                    return laplace_fx(cfix(1.0),orange_input_23_)
                                                  def anonymous_fun_139_(par_release_input_4_):
                                                    """
                                                    par_release_input_4_: (Double,(Double,(Double,(Double,(Double,Double)))))
                                                    """
                                                    def anonymous_fun_140_(orange_input_24_):
                                                      """
                                                      orange_input_24_: Double
                                                      """
                                                      return laplace_fx(cfix(1.0),orange_input_24_)
                                                    def anonymous_fun_141_(par_release_input_3_):
                                                      """
                                                      par_release_input_3_: (Double,(Double,(Double,(Double,Double))))
                                                      """
                                                      def anonymous_fun_142_(orange_input_25_):
                                                        """
                                                        orange_input_25_: Double
                                                        """
                                                        return laplace_fx(cfix(1.0),orange_input_25_)
                                                      def anonymous_fun_143_(par_release_input_2_):
                                                        """
                                                        par_release_input_2_: (Double,(Double,(Double,Double)))
                                                        """
                                                        def anonymous_fun_144_(orange_input_26_):
                                                          """
                                                          orange_input_26_: Double
                                                          """
                                                          return laplace_fx(cfix(1.0),orange_input_26_)
                                                        def anonymous_fun_145_(par_release_input_1_):
                                                          """
                                                          par_release_input_1_: (Double,(Double,Double))
                                                          """
                                                          def anonymous_fun_146_(orange_input_27_):
                                                            """
                                                            orange_input_27_: Double
                                                            """
                                                            return laplace_fx(cfix(1.0),orange_input_27_)
                                                          def anonymous_fun_147_(par_release_input_0_):
                                                            """
                                                            par_release_input_0_: (Double,Double)
                                                            """
                                                            def anonymous_fun_148_(orange_input_28_):
                                                              """
                                                              orange_input_28_: Double
                                                              """
                                                              return laplace_fx(cfix(1.0),orange_input_28_)
                                                            def anonymous_fun_149_(orange_input_29_):
                                                              """
                                                              orange_input_29_: Double
                                                              """
                                                              return laplace_fx(cfix(1.0),orange_input_29_)
                                                            return (anonymous_fun_148_(par_release_input_0_[0]),anonymous_fun_149_(par_release_input_0_[1]))
                                                          return (anonymous_fun_146_(par_release_input_1_[0]),anonymous_fun_147_(par_release_input_1_[1]))
                                                        return (anonymous_fun_144_(par_release_input_2_[0]),anonymous_fun_145_(par_release_input_2_[1]))
                                                      return (anonymous_fun_142_(par_release_input_3_[0]),anonymous_fun_143_(par_release_input_3_[1]))
                                                    return (anonymous_fun_140_(par_release_input_4_[0]),anonymous_fun_141_(par_release_input_4_[1]))
                                                  return (anonymous_fun_138_(par_release_input_5_[0]),anonymous_fun_139_(par_release_input_5_[1]))
                                                return (anonymous_fun_136_(par_release_input_6_[0]),anonymous_fun_137_(par_release_input_6_[1]))
                                              return (anonymous_fun_134_(par_release_input_7_[0]),anonymous_fun_135_(par_release_input_7_[1]))
                                            return (anonymous_fun_132_(par_release_input_8_[0]),anonymous_fun_133_(par_release_input_8_[1]))
                                          return (anonymous_fun_130_(par_release_input_9_[0]),anonymous_fun_131_(par_release_input_9_[1]))
                                        return (anonymous_fun_128_(par_release_input_10_[0]),anonymous_fun_129_(par_release_input_10_[1]))
                                      return (anonymous_fun_126_(par_release_input_11_[0]),anonymous_fun_127_(par_release_input_11_[1]))
                                    return (anonymous_fun_124_(par_release_input_12_[0]),anonymous_fun_125_(par_release_input_12_[1]))
                                  return (anonymous_fun_122_(par_release_input_13_[0]),anonymous_fun_123_(par_release_input_13_[1]))
                                return (anonymous_fun_120_(par_release_input_14_[0]),anonymous_fun_121_(par_release_input_14_[1]))
                              return (anonymous_fun_118_(par_release_input_15_[0]),anonymous_fun_119_(par_release_input_15_[1]))
                            return (anonymous_fun_116_(par_release_input_16_[0]),anonymous_fun_117_(par_release_input_16_[1]))
                          return (anonymous_fun_114_(par_release_input_17_[0]),anonymous_fun_115_(par_release_input_17_[1]))
                        return (anonymous_fun_112_(par_release_input_18_[0]),anonymous_fun_113_(par_release_input_18_[1]))
                      return (anonymous_fun_110_(par_release_input_19_[0]),anonymous_fun_111_(par_release_input_19_[1]))
                    return (anonymous_fun_108_(par_release_input_20_[0]),anonymous_fun_109_(par_release_input_20_[1]))
                  return (anonymous_fun_106_(par_release_input_21_[0]),anonymous_fun_107_(par_release_input_21_[1]))
                return (anonymous_fun_104_(par_release_input_22_[0]),anonymous_fun_105_(par_release_input_22_[1]))
              return (anonymous_fun_102_(par_release_input_23_[0]),anonymous_fun_103_(par_release_input_23_[1]))
            return (anonymous_fun_100_(par_release_input_24_[0]),anonymous_fun_101_(par_release_input_24_[1]))
          return (anonymous_fun_98_(par_release_input_25_[0]),anonymous_fun_99_(par_release_input_25_[1]))
        return (anonymous_fun_96_(par_release_input_26_[0]),anonymous_fun_97_(par_release_input_26_[1]))
      return (anonymous_fun_94_(par_release_input_27_[0]),anonymous_fun_95_(par_release_input_27_[1]))
    return (anonymous_fun_92_(par_release_input_28_[0]),anonymous_fun_93_(par_release_input_28_[1]))
  return anonymous_fun_91_
fused_vector_tup_0_ = bmcs(30,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_0_,(),anonymous_fun_90_)
[fused_vector_tup_0_[0],(fused_vector_tup_0_[1])[0],((fused_vector_tup_0_[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[0],((((fused_vector_tup_0_[1])[1])[1])[1])[0],(((((fused_vector_tup_0_[1])[1])[1])[1])[1])[0],((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[0],(((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],(((((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((((((((((((((((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1])[1]]