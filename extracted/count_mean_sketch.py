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
  def anonymous_fun_1_(par_map_input_8_):
    """
    par_map_input_8_: (Double,Int)
    """
    def anonymous_fun_2_(row_9_):
      """
      row_9_: (Double,Int)
      """
      def anonymous_fun_3_(num_90_):
        """
        num_90_: Double
        """
        if 0.0 <= num_90_ and num_90_ < 1.0:
          cond_result_1_ = 0
        else:
          cond_result_1_ = 0
        return cond_result_1_
      def anonymous_fun_4_(num_91_):
        """
        num_91_: Double
        """
        if 1.0 <= num_91_ and num_91_ < 2.0:
          cond_result_2_ = 1
        else:
          cond_result_2_ = 0
        return cond_result_2_
      def anonymous_fun_5_(num_92_):
        """
        num_92_: Double
        """
        if 2.0 <= num_92_ and num_92_ < 3.0:
          cond_result_3_ = 2
        else:
          cond_result_3_ = 0
        return cond_result_3_
      def anonymous_fun_6_(num_93_):
        """
        num_93_: Double
        """
        if 3.0 <= num_93_ and num_93_ < 4.0:
          cond_result_4_ = 3
        else:
          cond_result_4_ = 0
        return cond_result_4_
      def anonymous_fun_7_(num_94_):
        """
        num_94_: Double
        """
        if 4.0 <= num_94_ and num_94_ < 5.0:
          cond_result_5_ = 4
        else:
          cond_result_5_ = 0
        return cond_result_5_
      def anonymous_fun_8_(num_95_):
        """
        num_95_: Double
        """
        if 5.0 <= num_95_ and num_95_ < 6.0:
          cond_result_6_ = 5
        else:
          cond_result_6_ = 0
        return cond_result_6_
      def anonymous_fun_9_(num_96_):
        """
        num_96_: Double
        """
        if 6.0 <= num_96_ and num_96_ < 7.0:
          cond_result_7_ = 6
        else:
          cond_result_7_ = 0
        return cond_result_7_
      def anonymous_fun_10_(num_97_):
        """
        num_97_: Double
        """
        if 7.0 <= num_97_ and num_97_ < 8.0:
          cond_result_8_ = 7
        else:
          cond_result_8_ = 0
        return cond_result_8_
      def anonymous_fun_11_(num_98_):
        """
        num_98_: Double
        """
        if 8.0 <= num_98_ and num_98_ < 9.0:
          cond_result_9_ = 8
        else:
          cond_result_9_ = 0
        return cond_result_9_
      def anonymous_fun_12_(num_99_):
        """
        num_99_: Double
        """
        if 9.0 <= num_99_ and num_99_ < 10.0:
          cond_result_10_ = 9
        else:
          cond_result_10_ = 0
        return cond_result_10_
      if ([anonymous_fun_3_,anonymous_fun_4_,anonymous_fun_5_,anonymous_fun_6_,anonymous_fun_7_,anonymous_fun_8_,anonymous_fun_9_,anonymous_fun_10_,anonymous_fun_11_,anonymous_fun_12_][row_9_[1]])(row_9_[0]) == 0:
        cond_result_0_ = row_9_[0]
      else:
        cond_result_0_ = 0.0
      return cond_result_0_
    def anonymous_fun_13_(dbrow_0_):
      """
      dbrow_0_: (Double,Int)
      """
      return dbrow_0_
    def anonymous_fun_14_(par_map_input_7_):
      """
      par_map_input_7_: (Double,Int)
      """
      def anonymous_fun_15_(row_8_):
        """
        row_8_: (Double,Int)
        """
        def anonymous_fun_16_(num_80_):
          """
          num_80_: Double
          """
          if 0.0 <= num_80_ and num_80_ < 1.0:
            cond_result_12_ = 0
          else:
            cond_result_12_ = 0
          return cond_result_12_
        def anonymous_fun_17_(num_81_):
          """
          num_81_: Double
          """
          if 1.0 <= num_81_ and num_81_ < 2.0:
            cond_result_13_ = 1
          else:
            cond_result_13_ = 0
          return cond_result_13_
        def anonymous_fun_18_(num_82_):
          """
          num_82_: Double
          """
          if 2.0 <= num_82_ and num_82_ < 3.0:
            cond_result_14_ = 2
          else:
            cond_result_14_ = 0
          return cond_result_14_
        def anonymous_fun_19_(num_83_):
          """
          num_83_: Double
          """
          if 3.0 <= num_83_ and num_83_ < 4.0:
            cond_result_15_ = 3
          else:
            cond_result_15_ = 0
          return cond_result_15_
        def anonymous_fun_20_(num_84_):
          """
          num_84_: Double
          """
          if 4.0 <= num_84_ and num_84_ < 5.0:
            cond_result_16_ = 4
          else:
            cond_result_16_ = 0
          return cond_result_16_
        def anonymous_fun_21_(num_85_):
          """
          num_85_: Double
          """
          if 5.0 <= num_85_ and num_85_ < 6.0:
            cond_result_17_ = 5
          else:
            cond_result_17_ = 0
          return cond_result_17_
        def anonymous_fun_22_(num_86_):
          """
          num_86_: Double
          """
          if 6.0 <= num_86_ and num_86_ < 7.0:
            cond_result_18_ = 6
          else:
            cond_result_18_ = 0
          return cond_result_18_
        def anonymous_fun_23_(num_87_):
          """
          num_87_: Double
          """
          if 7.0 <= num_87_ and num_87_ < 8.0:
            cond_result_19_ = 7
          else:
            cond_result_19_ = 0
          return cond_result_19_
        def anonymous_fun_24_(num_88_):
          """
          num_88_: Double
          """
          if 8.0 <= num_88_ and num_88_ < 9.0:
            cond_result_20_ = 8
          else:
            cond_result_20_ = 0
          return cond_result_20_
        def anonymous_fun_25_(num_89_):
          """
          num_89_: Double
          """
          if 9.0 <= num_89_ and num_89_ < 10.0:
            cond_result_21_ = 9
          else:
            cond_result_21_ = 0
          return cond_result_21_
        if ([anonymous_fun_16_,anonymous_fun_17_,anonymous_fun_18_,anonymous_fun_19_,anonymous_fun_20_,anonymous_fun_21_,anonymous_fun_22_,anonymous_fun_23_,anonymous_fun_24_,anonymous_fun_25_][row_8_[1]])(row_8_[0]) == 1:
          cond_result_11_ = row_8_[0]
        else:
          cond_result_11_ = 0.0
        return cond_result_11_
      def anonymous_fun_26_(dbrow_1_):
        """
        dbrow_1_: (Double,Int)
        """
        return dbrow_1_
      def anonymous_fun_27_(par_map_input_6_):
        """
        par_map_input_6_: (Double,Int)
        """
        def anonymous_fun_28_(row_7_):
          """
          row_7_: (Double,Int)
          """
          def anonymous_fun_29_(num_70_):
            """
            num_70_: Double
            """
            if 0.0 <= num_70_ and num_70_ < 1.0:
              cond_result_23_ = 0
            else:
              cond_result_23_ = 0
            return cond_result_23_
          def anonymous_fun_30_(num_71_):
            """
            num_71_: Double
            """
            if 1.0 <= num_71_ and num_71_ < 2.0:
              cond_result_24_ = 1
            else:
              cond_result_24_ = 0
            return cond_result_24_
          def anonymous_fun_31_(num_72_):
            """
            num_72_: Double
            """
            if 2.0 <= num_72_ and num_72_ < 3.0:
              cond_result_25_ = 2
            else:
              cond_result_25_ = 0
            return cond_result_25_
          def anonymous_fun_32_(num_73_):
            """
            num_73_: Double
            """
            if 3.0 <= num_73_ and num_73_ < 4.0:
              cond_result_26_ = 3
            else:
              cond_result_26_ = 0
            return cond_result_26_
          def anonymous_fun_33_(num_74_):
            """
            num_74_: Double
            """
            if 4.0 <= num_74_ and num_74_ < 5.0:
              cond_result_27_ = 4
            else:
              cond_result_27_ = 0
            return cond_result_27_
          def anonymous_fun_34_(num_75_):
            """
            num_75_: Double
            """
            if 5.0 <= num_75_ and num_75_ < 6.0:
              cond_result_28_ = 5
            else:
              cond_result_28_ = 0
            return cond_result_28_
          def anonymous_fun_35_(num_76_):
            """
            num_76_: Double
            """
            if 6.0 <= num_76_ and num_76_ < 7.0:
              cond_result_29_ = 6
            else:
              cond_result_29_ = 0
            return cond_result_29_
          def anonymous_fun_36_(num_77_):
            """
            num_77_: Double
            """
            if 7.0 <= num_77_ and num_77_ < 8.0:
              cond_result_30_ = 7
            else:
              cond_result_30_ = 0
            return cond_result_30_
          def anonymous_fun_37_(num_78_):
            """
            num_78_: Double
            """
            if 8.0 <= num_78_ and num_78_ < 9.0:
              cond_result_31_ = 8
            else:
              cond_result_31_ = 0
            return cond_result_31_
          def anonymous_fun_38_(num_79_):
            """
            num_79_: Double
            """
            if 9.0 <= num_79_ and num_79_ < 10.0:
              cond_result_32_ = 9
            else:
              cond_result_32_ = 0
            return cond_result_32_
          if ([anonymous_fun_29_,anonymous_fun_30_,anonymous_fun_31_,anonymous_fun_32_,anonymous_fun_33_,anonymous_fun_34_,anonymous_fun_35_,anonymous_fun_36_,anonymous_fun_37_,anonymous_fun_38_][row_7_[1]])(row_7_[0]) == 2:
            cond_result_22_ = row_7_[0]
          else:
            cond_result_22_ = 0.0
          return cond_result_22_
        def anonymous_fun_39_(dbrow_2_):
          """
          dbrow_2_: (Double,Int)
          """
          return dbrow_2_
        def anonymous_fun_40_(par_map_input_5_):
          """
          par_map_input_5_: (Double,Int)
          """
          def anonymous_fun_41_(row_6_):
            """
            row_6_: (Double,Int)
            """
            def anonymous_fun_42_(num_60_):
              """
              num_60_: Double
              """
              if 0.0 <= num_60_ and num_60_ < 1.0:
                cond_result_34_ = 0
              else:
                cond_result_34_ = 0
              return cond_result_34_
            def anonymous_fun_43_(num_61_):
              """
              num_61_: Double
              """
              if 1.0 <= num_61_ and num_61_ < 2.0:
                cond_result_35_ = 1
              else:
                cond_result_35_ = 0
              return cond_result_35_
            def anonymous_fun_44_(num_62_):
              """
              num_62_: Double
              """
              if 2.0 <= num_62_ and num_62_ < 3.0:
                cond_result_36_ = 2
              else:
                cond_result_36_ = 0
              return cond_result_36_
            def anonymous_fun_45_(num_63_):
              """
              num_63_: Double
              """
              if 3.0 <= num_63_ and num_63_ < 4.0:
                cond_result_37_ = 3
              else:
                cond_result_37_ = 0
              return cond_result_37_
            def anonymous_fun_46_(num_64_):
              """
              num_64_: Double
              """
              if 4.0 <= num_64_ and num_64_ < 5.0:
                cond_result_38_ = 4
              else:
                cond_result_38_ = 0
              return cond_result_38_
            def anonymous_fun_47_(num_65_):
              """
              num_65_: Double
              """
              if 5.0 <= num_65_ and num_65_ < 6.0:
                cond_result_39_ = 5
              else:
                cond_result_39_ = 0
              return cond_result_39_
            def anonymous_fun_48_(num_66_):
              """
              num_66_: Double
              """
              if 6.0 <= num_66_ and num_66_ < 7.0:
                cond_result_40_ = 6
              else:
                cond_result_40_ = 0
              return cond_result_40_
            def anonymous_fun_49_(num_67_):
              """
              num_67_: Double
              """
              if 7.0 <= num_67_ and num_67_ < 8.0:
                cond_result_41_ = 7
              else:
                cond_result_41_ = 0
              return cond_result_41_
            def anonymous_fun_50_(num_68_):
              """
              num_68_: Double
              """
              if 8.0 <= num_68_ and num_68_ < 9.0:
                cond_result_42_ = 8
              else:
                cond_result_42_ = 0
              return cond_result_42_
            def anonymous_fun_51_(num_69_):
              """
              num_69_: Double
              """
              if 9.0 <= num_69_ and num_69_ < 10.0:
                cond_result_43_ = 9
              else:
                cond_result_43_ = 0
              return cond_result_43_
            if ([anonymous_fun_42_,anonymous_fun_43_,anonymous_fun_44_,anonymous_fun_45_,anonymous_fun_46_,anonymous_fun_47_,anonymous_fun_48_,anonymous_fun_49_,anonymous_fun_50_,anonymous_fun_51_][row_6_[1]])(row_6_[0]) == 3:
              cond_result_33_ = row_6_[0]
            else:
              cond_result_33_ = 0.0
            return cond_result_33_
          def anonymous_fun_52_(dbrow_3_):
            """
            dbrow_3_: (Double,Int)
            """
            return dbrow_3_
          def anonymous_fun_53_(par_map_input_4_):
            """
            par_map_input_4_: (Double,Int)
            """
            def anonymous_fun_54_(row_5_):
              """
              row_5_: (Double,Int)
              """
              def anonymous_fun_55_(num_50_):
                """
                num_50_: Double
                """
                if 0.0 <= num_50_ and num_50_ < 1.0:
                  cond_result_45_ = 0
                else:
                  cond_result_45_ = 0
                return cond_result_45_
              def anonymous_fun_56_(num_51_):
                """
                num_51_: Double
                """
                if 1.0 <= num_51_ and num_51_ < 2.0:
                  cond_result_46_ = 1
                else:
                  cond_result_46_ = 0
                return cond_result_46_
              def anonymous_fun_57_(num_52_):
                """
                num_52_: Double
                """
                if 2.0 <= num_52_ and num_52_ < 3.0:
                  cond_result_47_ = 2
                else:
                  cond_result_47_ = 0
                return cond_result_47_
              def anonymous_fun_58_(num_53_):
                """
                num_53_: Double
                """
                if 3.0 <= num_53_ and num_53_ < 4.0:
                  cond_result_48_ = 3
                else:
                  cond_result_48_ = 0
                return cond_result_48_
              def anonymous_fun_59_(num_54_):
                """
                num_54_: Double
                """
                if 4.0 <= num_54_ and num_54_ < 5.0:
                  cond_result_49_ = 4
                else:
                  cond_result_49_ = 0
                return cond_result_49_
              def anonymous_fun_60_(num_55_):
                """
                num_55_: Double
                """
                if 5.0 <= num_55_ and num_55_ < 6.0:
                  cond_result_50_ = 5
                else:
                  cond_result_50_ = 0
                return cond_result_50_
              def anonymous_fun_61_(num_56_):
                """
                num_56_: Double
                """
                if 6.0 <= num_56_ and num_56_ < 7.0:
                  cond_result_51_ = 6
                else:
                  cond_result_51_ = 0
                return cond_result_51_
              def anonymous_fun_62_(num_57_):
                """
                num_57_: Double
                """
                if 7.0 <= num_57_ and num_57_ < 8.0:
                  cond_result_52_ = 7
                else:
                  cond_result_52_ = 0
                return cond_result_52_
              def anonymous_fun_63_(num_58_):
                """
                num_58_: Double
                """
                if 8.0 <= num_58_ and num_58_ < 9.0:
                  cond_result_53_ = 8
                else:
                  cond_result_53_ = 0
                return cond_result_53_
              def anonymous_fun_64_(num_59_):
                """
                num_59_: Double
                """
                if 9.0 <= num_59_ and num_59_ < 10.0:
                  cond_result_54_ = 9
                else:
                  cond_result_54_ = 0
                return cond_result_54_
              if ([anonymous_fun_55_,anonymous_fun_56_,anonymous_fun_57_,anonymous_fun_58_,anonymous_fun_59_,anonymous_fun_60_,anonymous_fun_61_,anonymous_fun_62_,anonymous_fun_63_,anonymous_fun_64_][row_5_[1]])(row_5_[0]) == 4:
                cond_result_44_ = row_5_[0]
              else:
                cond_result_44_ = 0.0
              return cond_result_44_
            def anonymous_fun_65_(dbrow_4_):
              """
              dbrow_4_: (Double,Int)
              """
              return dbrow_4_
            def anonymous_fun_66_(par_map_input_3_):
              """
              par_map_input_3_: (Double,Int)
              """
              def anonymous_fun_67_(row_4_):
                """
                row_4_: (Double,Int)
                """
                def anonymous_fun_68_(num_40_):
                  """
                  num_40_: Double
                  """
                  if 0.0 <= num_40_ and num_40_ < 1.0:
                    cond_result_56_ = 0
                  else:
                    cond_result_56_ = 0
                  return cond_result_56_
                def anonymous_fun_69_(num_41_):
                  """
                  num_41_: Double
                  """
                  if 1.0 <= num_41_ and num_41_ < 2.0:
                    cond_result_57_ = 1
                  else:
                    cond_result_57_ = 0
                  return cond_result_57_
                def anonymous_fun_70_(num_42_):
                  """
                  num_42_: Double
                  """
                  if 2.0 <= num_42_ and num_42_ < 3.0:
                    cond_result_58_ = 2
                  else:
                    cond_result_58_ = 0
                  return cond_result_58_
                def anonymous_fun_71_(num_43_):
                  """
                  num_43_: Double
                  """
                  if 3.0 <= num_43_ and num_43_ < 4.0:
                    cond_result_59_ = 3
                  else:
                    cond_result_59_ = 0
                  return cond_result_59_
                def anonymous_fun_72_(num_44_):
                  """
                  num_44_: Double
                  """
                  if 4.0 <= num_44_ and num_44_ < 5.0:
                    cond_result_60_ = 4
                  else:
                    cond_result_60_ = 0
                  return cond_result_60_
                def anonymous_fun_73_(num_45_):
                  """
                  num_45_: Double
                  """
                  if 5.0 <= num_45_ and num_45_ < 6.0:
                    cond_result_61_ = 5
                  else:
                    cond_result_61_ = 0
                  return cond_result_61_
                def anonymous_fun_74_(num_46_):
                  """
                  num_46_: Double
                  """
                  if 6.0 <= num_46_ and num_46_ < 7.0:
                    cond_result_62_ = 6
                  else:
                    cond_result_62_ = 0
                  return cond_result_62_
                def anonymous_fun_75_(num_47_):
                  """
                  num_47_: Double
                  """
                  if 7.0 <= num_47_ and num_47_ < 8.0:
                    cond_result_63_ = 7
                  else:
                    cond_result_63_ = 0
                  return cond_result_63_
                def anonymous_fun_76_(num_48_):
                  """
                  num_48_: Double
                  """
                  if 8.0 <= num_48_ and num_48_ < 9.0:
                    cond_result_64_ = 8
                  else:
                    cond_result_64_ = 0
                  return cond_result_64_
                def anonymous_fun_77_(num_49_):
                  """
                  num_49_: Double
                  """
                  if 9.0 <= num_49_ and num_49_ < 10.0:
                    cond_result_65_ = 9
                  else:
                    cond_result_65_ = 0
                  return cond_result_65_
                if ([anonymous_fun_68_,anonymous_fun_69_,anonymous_fun_70_,anonymous_fun_71_,anonymous_fun_72_,anonymous_fun_73_,anonymous_fun_74_,anonymous_fun_75_,anonymous_fun_76_,anonymous_fun_77_][row_4_[1]])(row_4_[0]) == 5:
                  cond_result_55_ = row_4_[0]
                else:
                  cond_result_55_ = 0.0
                return cond_result_55_
              def anonymous_fun_78_(dbrow_5_):
                """
                dbrow_5_: (Double,Int)
                """
                return dbrow_5_
              def anonymous_fun_79_(par_map_input_2_):
                """
                par_map_input_2_: (Double,Int)
                """
                def anonymous_fun_80_(row_3_):
                  """
                  row_3_: (Double,Int)
                  """
                  def anonymous_fun_81_(num_30_):
                    """
                    num_30_: Double
                    """
                    if 0.0 <= num_30_ and num_30_ < 1.0:
                      cond_result_67_ = 0
                    else:
                      cond_result_67_ = 0
                    return cond_result_67_
                  def anonymous_fun_82_(num_31_):
                    """
                    num_31_: Double
                    """
                    if 1.0 <= num_31_ and num_31_ < 2.0:
                      cond_result_68_ = 1
                    else:
                      cond_result_68_ = 0
                    return cond_result_68_
                  def anonymous_fun_83_(num_32_):
                    """
                    num_32_: Double
                    """
                    if 2.0 <= num_32_ and num_32_ < 3.0:
                      cond_result_69_ = 2
                    else:
                      cond_result_69_ = 0
                    return cond_result_69_
                  def anonymous_fun_84_(num_33_):
                    """
                    num_33_: Double
                    """
                    if 3.0 <= num_33_ and num_33_ < 4.0:
                      cond_result_70_ = 3
                    else:
                      cond_result_70_ = 0
                    return cond_result_70_
                  def anonymous_fun_85_(num_34_):
                    """
                    num_34_: Double
                    """
                    if 4.0 <= num_34_ and num_34_ < 5.0:
                      cond_result_71_ = 4
                    else:
                      cond_result_71_ = 0
                    return cond_result_71_
                  def anonymous_fun_86_(num_35_):
                    """
                    num_35_: Double
                    """
                    if 5.0 <= num_35_ and num_35_ < 6.0:
                      cond_result_72_ = 5
                    else:
                      cond_result_72_ = 0
                    return cond_result_72_
                  def anonymous_fun_87_(num_36_):
                    """
                    num_36_: Double
                    """
                    if 6.0 <= num_36_ and num_36_ < 7.0:
                      cond_result_73_ = 6
                    else:
                      cond_result_73_ = 0
                    return cond_result_73_
                  def anonymous_fun_88_(num_37_):
                    """
                    num_37_: Double
                    """
                    if 7.0 <= num_37_ and num_37_ < 8.0:
                      cond_result_74_ = 7
                    else:
                      cond_result_74_ = 0
                    return cond_result_74_
                  def anonymous_fun_89_(num_38_):
                    """
                    num_38_: Double
                    """
                    if 8.0 <= num_38_ and num_38_ < 9.0:
                      cond_result_75_ = 8
                    else:
                      cond_result_75_ = 0
                    return cond_result_75_
                  def anonymous_fun_90_(num_39_):
                    """
                    num_39_: Double
                    """
                    if 9.0 <= num_39_ and num_39_ < 10.0:
                      cond_result_76_ = 9
                    else:
                      cond_result_76_ = 0
                    return cond_result_76_
                  if ([anonymous_fun_81_,anonymous_fun_82_,anonymous_fun_83_,anonymous_fun_84_,anonymous_fun_85_,anonymous_fun_86_,anonymous_fun_87_,anonymous_fun_88_,anonymous_fun_89_,anonymous_fun_90_][row_3_[1]])(row_3_[0]) == 6:
                    cond_result_66_ = row_3_[0]
                  else:
                    cond_result_66_ = 0.0
                  return cond_result_66_
                def anonymous_fun_91_(dbrow_6_):
                  """
                  dbrow_6_: (Double,Int)
                  """
                  return dbrow_6_
                def anonymous_fun_92_(par_map_input_1_):
                  """
                  par_map_input_1_: (Double,Int)
                  """
                  def anonymous_fun_93_(row_2_):
                    """
                    row_2_: (Double,Int)
                    """
                    def anonymous_fun_94_(num_20_):
                      """
                      num_20_: Double
                      """
                      if 0.0 <= num_20_ and num_20_ < 1.0:
                        cond_result_78_ = 0
                      else:
                        cond_result_78_ = 0
                      return cond_result_78_
                    def anonymous_fun_95_(num_21_):
                      """
                      num_21_: Double
                      """
                      if 1.0 <= num_21_ and num_21_ < 2.0:
                        cond_result_79_ = 1
                      else:
                        cond_result_79_ = 0
                      return cond_result_79_
                    def anonymous_fun_96_(num_22_):
                      """
                      num_22_: Double
                      """
                      if 2.0 <= num_22_ and num_22_ < 3.0:
                        cond_result_80_ = 2
                      else:
                        cond_result_80_ = 0
                      return cond_result_80_
                    def anonymous_fun_97_(num_23_):
                      """
                      num_23_: Double
                      """
                      if 3.0 <= num_23_ and num_23_ < 4.0:
                        cond_result_81_ = 3
                      else:
                        cond_result_81_ = 0
                      return cond_result_81_
                    def anonymous_fun_98_(num_24_):
                      """
                      num_24_: Double
                      """
                      if 4.0 <= num_24_ and num_24_ < 5.0:
                        cond_result_82_ = 4
                      else:
                        cond_result_82_ = 0
                      return cond_result_82_
                    def anonymous_fun_99_(num_25_):
                      """
                      num_25_: Double
                      """
                      if 5.0 <= num_25_ and num_25_ < 6.0:
                        cond_result_83_ = 5
                      else:
                        cond_result_83_ = 0
                      return cond_result_83_
                    def anonymous_fun_100_(num_26_):
                      """
                      num_26_: Double
                      """
                      if 6.0 <= num_26_ and num_26_ < 7.0:
                        cond_result_84_ = 6
                      else:
                        cond_result_84_ = 0
                      return cond_result_84_
                    def anonymous_fun_101_(num_27_):
                      """
                      num_27_: Double
                      """
                      if 7.0 <= num_27_ and num_27_ < 8.0:
                        cond_result_85_ = 7
                      else:
                        cond_result_85_ = 0
                      return cond_result_85_
                    def anonymous_fun_102_(num_28_):
                      """
                      num_28_: Double
                      """
                      if 8.0 <= num_28_ and num_28_ < 9.0:
                        cond_result_86_ = 8
                      else:
                        cond_result_86_ = 0
                      return cond_result_86_
                    def anonymous_fun_103_(num_29_):
                      """
                      num_29_: Double
                      """
                      if 9.0 <= num_29_ and num_29_ < 10.0:
                        cond_result_87_ = 9
                      else:
                        cond_result_87_ = 0
                      return cond_result_87_
                    if ([anonymous_fun_94_,anonymous_fun_95_,anonymous_fun_96_,anonymous_fun_97_,anonymous_fun_98_,anonymous_fun_99_,anonymous_fun_100_,anonymous_fun_101_,anonymous_fun_102_,anonymous_fun_103_][row_2_[1]])(row_2_[0]) == 7:
                      cond_result_77_ = row_2_[0]
                    else:
                      cond_result_77_ = 0.0
                    return cond_result_77_
                  def anonymous_fun_104_(dbrow_7_):
                    """
                    dbrow_7_: (Double,Int)
                    """
                    return dbrow_7_
                  def anonymous_fun_105_(par_map_input_0_):
                    """
                    par_map_input_0_: (Double,Int)
                    """
                    def anonymous_fun_106_(row_1_):
                      """
                      row_1_: (Double,Int)
                      """
                      def anonymous_fun_107_(num_10_):
                        """
                        num_10_: Double
                        """
                        if 0.0 <= num_10_ and num_10_ < 1.0:
                          cond_result_89_ = 0
                        else:
                          cond_result_89_ = 0
                        return cond_result_89_
                      def anonymous_fun_108_(num_11_):
                        """
                        num_11_: Double
                        """
                        if 1.0 <= num_11_ and num_11_ < 2.0:
                          cond_result_90_ = 1
                        else:
                          cond_result_90_ = 0
                        return cond_result_90_
                      def anonymous_fun_109_(num_12_):
                        """
                        num_12_: Double
                        """
                        if 2.0 <= num_12_ and num_12_ < 3.0:
                          cond_result_91_ = 2
                        else:
                          cond_result_91_ = 0
                        return cond_result_91_
                      def anonymous_fun_110_(num_13_):
                        """
                        num_13_: Double
                        """
                        if 3.0 <= num_13_ and num_13_ < 4.0:
                          cond_result_92_ = 3
                        else:
                          cond_result_92_ = 0
                        return cond_result_92_
                      def anonymous_fun_111_(num_14_):
                        """
                        num_14_: Double
                        """
                        if 4.0 <= num_14_ and num_14_ < 5.0:
                          cond_result_93_ = 4
                        else:
                          cond_result_93_ = 0
                        return cond_result_93_
                      def anonymous_fun_112_(num_15_):
                        """
                        num_15_: Double
                        """
                        if 5.0 <= num_15_ and num_15_ < 6.0:
                          cond_result_94_ = 5
                        else:
                          cond_result_94_ = 0
                        return cond_result_94_
                      def anonymous_fun_113_(num_16_):
                        """
                        num_16_: Double
                        """
                        if 6.0 <= num_16_ and num_16_ < 7.0:
                          cond_result_95_ = 6
                        else:
                          cond_result_95_ = 0
                        return cond_result_95_
                      def anonymous_fun_114_(num_17_):
                        """
                        num_17_: Double
                        """
                        if 7.0 <= num_17_ and num_17_ < 8.0:
                          cond_result_96_ = 7
                        else:
                          cond_result_96_ = 0
                        return cond_result_96_
                      def anonymous_fun_115_(num_18_):
                        """
                        num_18_: Double
                        """
                        if 8.0 <= num_18_ and num_18_ < 9.0:
                          cond_result_97_ = 8
                        else:
                          cond_result_97_ = 0
                        return cond_result_97_
                      def anonymous_fun_116_(num_19_):
                        """
                        num_19_: Double
                        """
                        if 9.0 <= num_19_ and num_19_ < 10.0:
                          cond_result_98_ = 9
                        else:
                          cond_result_98_ = 0
                        return cond_result_98_
                      if ([anonymous_fun_107_,anonymous_fun_108_,anonymous_fun_109_,anonymous_fun_110_,anonymous_fun_111_,anonymous_fun_112_,anonymous_fun_113_,anonymous_fun_114_,anonymous_fun_115_,anonymous_fun_116_][row_1_[1]])(row_1_[0]) == 8:
                        cond_result_88_ = row_1_[0]
                      else:
                        cond_result_88_ = 0.0
                      return cond_result_88_
                    def anonymous_fun_117_(dbrow_8_):
                      """
                      dbrow_8_: (Double,Int)
                      """
                      return dbrow_8_
                    def anonymous_fun_118_(row_0_):
                      """
                      row_0_: (Double,Int)
                      """
                      def anonymous_fun_119_(num_0_):
                        """
                        num_0_: Double
                        """
                        if 0.0 <= num_0_ and num_0_ < 1.0:
                          cond_result_100_ = 0
                        else:
                          cond_result_100_ = 0
                        return cond_result_100_
                      def anonymous_fun_120_(num_1_):
                        """
                        num_1_: Double
                        """
                        if 1.0 <= num_1_ and num_1_ < 2.0:
                          cond_result_101_ = 1
                        else:
                          cond_result_101_ = 0
                        return cond_result_101_
                      def anonymous_fun_121_(num_2_):
                        """
                        num_2_: Double
                        """
                        if 2.0 <= num_2_ and num_2_ < 3.0:
                          cond_result_102_ = 2
                        else:
                          cond_result_102_ = 0
                        return cond_result_102_
                      def anonymous_fun_122_(num_3_):
                        """
                        num_3_: Double
                        """
                        if 3.0 <= num_3_ and num_3_ < 4.0:
                          cond_result_103_ = 3
                        else:
                          cond_result_103_ = 0
                        return cond_result_103_
                      def anonymous_fun_123_(num_4_):
                        """
                        num_4_: Double
                        """
                        if 4.0 <= num_4_ and num_4_ < 5.0:
                          cond_result_104_ = 4
                        else:
                          cond_result_104_ = 0
                        return cond_result_104_
                      def anonymous_fun_124_(num_5_):
                        """
                        num_5_: Double
                        """
                        if 5.0 <= num_5_ and num_5_ < 6.0:
                          cond_result_105_ = 5
                        else:
                          cond_result_105_ = 0
                        return cond_result_105_
                      def anonymous_fun_125_(num_6_):
                        """
                        num_6_: Double
                        """
                        if 6.0 <= num_6_ and num_6_ < 7.0:
                          cond_result_106_ = 6
                        else:
                          cond_result_106_ = 0
                        return cond_result_106_
                      def anonymous_fun_126_(num_7_):
                        """
                        num_7_: Double
                        """
                        if 7.0 <= num_7_ and num_7_ < 8.0:
                          cond_result_107_ = 7
                        else:
                          cond_result_107_ = 0
                        return cond_result_107_
                      def anonymous_fun_127_(num_8_):
                        """
                        num_8_: Double
                        """
                        if 8.0 <= num_8_ and num_8_ < 9.0:
                          cond_result_108_ = 8
                        else:
                          cond_result_108_ = 0
                        return cond_result_108_
                      def anonymous_fun_128_(num_9_):
                        """
                        num_9_: Double
                        """
                        if 9.0 <= num_9_ and num_9_ < 10.0:
                          cond_result_109_ = 9
                        else:
                          cond_result_109_ = 0
                        return cond_result_109_
                      if ([anonymous_fun_119_,anonymous_fun_120_,anonymous_fun_121_,anonymous_fun_122_,anonymous_fun_123_,anonymous_fun_124_,anonymous_fun_125_,anonymous_fun_126_,anonymous_fun_127_,anonymous_fun_128_][row_0_[1]])(row_0_[0]) == 9:
                        cond_result_99_ = row_0_[0]
                      else:
                        cond_result_99_ = 0.0
                      return cond_result_99_
                    def anonymous_fun_129_(dbrow_9_):
                      """
                      dbrow_9_: (Double,Int)
                      """
                      return dbrow_9_
                    return ((fun_comp(anonymous_fun_106_,anonymous_fun_117_))(par_map_input_0_),(fun_comp(anonymous_fun_118_,anonymous_fun_129_))(par_map_input_0_))
                  return ((fun_comp(anonymous_fun_93_,anonymous_fun_104_))(par_map_input_1_),anonymous_fun_105_(par_map_input_1_))
                return ((fun_comp(anonymous_fun_80_,anonymous_fun_91_))(par_map_input_2_),anonymous_fun_92_(par_map_input_2_))
              return ((fun_comp(anonymous_fun_67_,anonymous_fun_78_))(par_map_input_3_),anonymous_fun_79_(par_map_input_3_))
            return ((fun_comp(anonymous_fun_54_,anonymous_fun_65_))(par_map_input_4_),anonymous_fun_66_(par_map_input_4_))
          return ((fun_comp(anonymous_fun_41_,anonymous_fun_52_))(par_map_input_5_),anonymous_fun_53_(par_map_input_5_))
        return ((fun_comp(anonymous_fun_28_,anonymous_fun_39_))(par_map_input_6_),anonymous_fun_40_(par_map_input_6_))
      return ((fun_comp(anonymous_fun_15_,anonymous_fun_26_))(par_map_input_7_),anonymous_fun_27_(par_map_input_7_))
    return ((fun_comp(anonymous_fun_2_,anonymous_fun_13_))(par_map_input_8_),anonymous_fun_14_(par_map_input_8_))
  return anonymous_fun_1_
def anonymous_fun_130_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_131_(par_release_input_8_):
    """
    par_release_input_8_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))
    """
    def anonymous_fun_132_(orange_input_0_):
      """
      orange_input_0_: Double
      """
      return laplace_fx(cfix(1.0),orange_input_0_)
    def anonymous_fun_133_(par_release_input_7_):
      """
      par_release_input_7_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))
      """
      def anonymous_fun_134_(orange_input_1_):
        """
        orange_input_1_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_1_)
      def anonymous_fun_135_(par_release_input_6_):
        """
        par_release_input_6_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))
        """
        def anonymous_fun_136_(orange_input_2_):
          """
          orange_input_2_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_2_)
        def anonymous_fun_137_(par_release_input_5_):
          """
          par_release_input_5_: (Double,(Double,(Double,(Double,(Double,(Double,Double))))))
          """
          def anonymous_fun_138_(orange_input_3_):
            """
            orange_input_3_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_3_)
          def anonymous_fun_139_(par_release_input_4_):
            """
            par_release_input_4_: (Double,(Double,(Double,(Double,(Double,Double)))))
            """
            def anonymous_fun_140_(orange_input_4_):
              """
              orange_input_4_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_4_)
            def anonymous_fun_141_(par_release_input_3_):
              """
              par_release_input_3_: (Double,(Double,(Double,(Double,Double))))
              """
              def anonymous_fun_142_(orange_input_5_):
                """
                orange_input_5_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_5_)
              def anonymous_fun_143_(par_release_input_2_):
                """
                par_release_input_2_: (Double,(Double,(Double,Double)))
                """
                def anonymous_fun_144_(orange_input_6_):
                  """
                  orange_input_6_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_6_)
                def anonymous_fun_145_(par_release_input_1_):
                  """
                  par_release_input_1_: (Double,(Double,Double))
                  """
                  def anonymous_fun_146_(orange_input_7_):
                    """
                    orange_input_7_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_7_)
                  def anonymous_fun_147_(par_release_input_0_):
                    """
                    par_release_input_0_: (Double,Double)
                    """
                    def anonymous_fun_148_(orange_input_8_):
                      """
                      orange_input_8_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_8_)
                    def anonymous_fun_149_(orange_input_9_):
                      """
                      orange_input_9_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_9_)
                    return (anonymous_fun_148_(par_release_input_0_[0]),anonymous_fun_149_(par_release_input_0_[1]))
                  return (anonymous_fun_146_(par_release_input_1_[0]),anonymous_fun_147_(par_release_input_1_[1]))
                return (anonymous_fun_144_(par_release_input_2_[0]),anonymous_fun_145_(par_release_input_2_[1]))
              return (anonymous_fun_142_(par_release_input_3_[0]),anonymous_fun_143_(par_release_input_3_[1]))
            return (anonymous_fun_140_(par_release_input_4_[0]),anonymous_fun_141_(par_release_input_4_[1]))
          return (anonymous_fun_138_(par_release_input_5_[0]),anonymous_fun_139_(par_release_input_5_[1]))
        return (anonymous_fun_136_(par_release_input_6_[0]),anonymous_fun_137_(par_release_input_6_[1]))
      return (anonymous_fun_134_(par_release_input_7_[0]),anonymous_fun_135_(par_release_input_7_[1]))
    return (anonymous_fun_132_(par_release_input_8_[0]),anonymous_fun_133_(par_release_input_8_[1]))
  return anonymous_fun_131_
fused_vector_tup_0_ = bmcs(10,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_0_,(),anonymous_fun_130_)
[fused_vector_tup_0_[0],(fused_vector_tup_0_[1])[0],((fused_vector_tup_0_[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[0],((((fused_vector_tup_0_[1])[1])[1])[1])[0],(((((fused_vector_tup_0_[1])[1])[1])[1])[1])[0],((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[0],(((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1]]