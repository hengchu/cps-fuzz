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
    par_map_input_8_: Vec Double
    """
    def anonymous_fun_2_(row_9_):
      """
      row_9_: Vec Double
      """
      return row_9_[0]
    def anonymous_fun_3_(dbrow_0_):
      """
      dbrow_0_: Vec Double
      """
      return dbrow_0_
    def anonymous_fun_4_(par_map_input_7_):
      """
      par_map_input_7_: Vec Double
      """
      def anonymous_fun_5_(row_8_):
        """
        row_8_: Vec Double
        """
        return row_8_[1]
      def anonymous_fun_6_(dbrow_1_):
        """
        dbrow_1_: Vec Double
        """
        return dbrow_1_
      def anonymous_fun_7_(par_map_input_6_):
        """
        par_map_input_6_: Vec Double
        """
        def anonymous_fun_8_(row_7_):
          """
          row_7_: Vec Double
          """
          return row_7_[2]
        def anonymous_fun_9_(dbrow_2_):
          """
          dbrow_2_: Vec Double
          """
          return dbrow_2_
        def anonymous_fun_10_(par_map_input_5_):
          """
          par_map_input_5_: Vec Double
          """
          def anonymous_fun_11_(row_6_):
            """
            row_6_: Vec Double
            """
            return row_6_[3]
          def anonymous_fun_12_(dbrow_3_):
            """
            dbrow_3_: Vec Double
            """
            return dbrow_3_
          def anonymous_fun_13_(par_map_input_4_):
            """
            par_map_input_4_: Vec Double
            """
            def anonymous_fun_14_(row_5_):
              """
              row_5_: Vec Double
              """
              return row_5_[4]
            def anonymous_fun_15_(dbrow_4_):
              """
              dbrow_4_: Vec Double
              """
              return dbrow_4_
            def anonymous_fun_16_(par_map_input_3_):
              """
              par_map_input_3_: Vec Double
              """
              def anonymous_fun_17_(row_4_):
                """
                row_4_: Vec Double
                """
                return row_4_[5]
              def anonymous_fun_18_(dbrow_5_):
                """
                dbrow_5_: Vec Double
                """
                return dbrow_5_
              def anonymous_fun_19_(par_map_input_2_):
                """
                par_map_input_2_: Vec Double
                """
                def anonymous_fun_20_(row_3_):
                  """
                  row_3_: Vec Double
                  """
                  return row_3_[6]
                def anonymous_fun_21_(dbrow_6_):
                  """
                  dbrow_6_: Vec Double
                  """
                  return dbrow_6_
                def anonymous_fun_22_(par_map_input_1_):
                  """
                  par_map_input_1_: Vec Double
                  """
                  def anonymous_fun_23_(row_2_):
                    """
                    row_2_: Vec Double
                    """
                    return row_2_[7]
                  def anonymous_fun_24_(dbrow_7_):
                    """
                    dbrow_7_: Vec Double
                    """
                    return dbrow_7_
                  def anonymous_fun_25_(par_map_input_0_):
                    """
                    par_map_input_0_: Vec Double
                    """
                    def anonymous_fun_26_(row_1_):
                      """
                      row_1_: Vec Double
                      """
                      return row_1_[8]
                    def anonymous_fun_27_(dbrow_8_):
                      """
                      dbrow_8_: Vec Double
                      """
                      return dbrow_8_
                    def anonymous_fun_28_(row_0_):
                      """
                      row_0_: Vec Double
                      """
                      return row_0_[9]
                    def anonymous_fun_29_(dbrow_9_):
                      """
                      dbrow_9_: Vec Double
                      """
                      return dbrow_9_
                    return ((fun_comp(anonymous_fun_26_,anonymous_fun_27_))(par_map_input_0_),(fun_comp(anonymous_fun_28_,anonymous_fun_29_))(par_map_input_0_))
                  return ((fun_comp(anonymous_fun_23_,anonymous_fun_24_))(par_map_input_1_),anonymous_fun_25_(par_map_input_1_))
                return ((fun_comp(anonymous_fun_20_,anonymous_fun_21_))(par_map_input_2_),anonymous_fun_22_(par_map_input_2_))
              return ((fun_comp(anonymous_fun_17_,anonymous_fun_18_))(par_map_input_3_),anonymous_fun_19_(par_map_input_3_))
            return ((fun_comp(anonymous_fun_14_,anonymous_fun_15_))(par_map_input_4_),anonymous_fun_16_(par_map_input_4_))
          return ((fun_comp(anonymous_fun_11_,anonymous_fun_12_))(par_map_input_5_),anonymous_fun_13_(par_map_input_5_))
        return ((fun_comp(anonymous_fun_8_,anonymous_fun_9_))(par_map_input_6_),anonymous_fun_10_(par_map_input_6_))
      return ((fun_comp(anonymous_fun_5_,anonymous_fun_6_))(par_map_input_7_),anonymous_fun_7_(par_map_input_7_))
    return ((fun_comp(anonymous_fun_2_,anonymous_fun_3_))(par_map_input_8_),anonymous_fun_4_(par_map_input_8_))
  return anonymous_fun_1_
def anonymous_fun_30_(empty_closure_1_):
  """
  empty_closure_1_: ()
  """
  def anonymous_fun_31_(par_release_input_8_):
    """
    par_release_input_8_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))))
    """
    def anonymous_fun_32_(orange_input_0_):
      """
      orange_input_0_: Double
      """
      return laplace_fx(cfix(1.0),orange_input_0_)
    def anonymous_fun_33_(par_release_input_7_):
      """
      par_release_input_7_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,(Double,Double))))))))
      """
      def anonymous_fun_34_(orange_input_1_):
        """
        orange_input_1_: Double
        """
        return laplace_fx(cfix(1.0),orange_input_1_)
      def anonymous_fun_35_(par_release_input_6_):
        """
        par_release_input_6_: (Double,(Double,(Double,(Double,(Double,(Double,(Double,Double)))))))
        """
        def anonymous_fun_36_(orange_input_2_):
          """
          orange_input_2_: Double
          """
          return laplace_fx(cfix(1.0),orange_input_2_)
        def anonymous_fun_37_(par_release_input_5_):
          """
          par_release_input_5_: (Double,(Double,(Double,(Double,(Double,(Double,Double))))))
          """
          def anonymous_fun_38_(orange_input_3_):
            """
            orange_input_3_: Double
            """
            return laplace_fx(cfix(1.0),orange_input_3_)
          def anonymous_fun_39_(par_release_input_4_):
            """
            par_release_input_4_: (Double,(Double,(Double,(Double,(Double,Double)))))
            """
            def anonymous_fun_40_(orange_input_4_):
              """
              orange_input_4_: Double
              """
              return laplace_fx(cfix(1.0),orange_input_4_)
            def anonymous_fun_41_(par_release_input_3_):
              """
              par_release_input_3_: (Double,(Double,(Double,(Double,Double))))
              """
              def anonymous_fun_42_(orange_input_5_):
                """
                orange_input_5_: Double
                """
                return laplace_fx(cfix(1.0),orange_input_5_)
              def anonymous_fun_43_(par_release_input_2_):
                """
                par_release_input_2_: (Double,(Double,(Double,Double)))
                """
                def anonymous_fun_44_(orange_input_6_):
                  """
                  orange_input_6_: Double
                  """
                  return laplace_fx(cfix(1.0),orange_input_6_)
                def anonymous_fun_45_(par_release_input_1_):
                  """
                  par_release_input_1_: (Double,(Double,Double))
                  """
                  def anonymous_fun_46_(orange_input_7_):
                    """
                    orange_input_7_: Double
                    """
                    return laplace_fx(cfix(1.0),orange_input_7_)
                  def anonymous_fun_47_(par_release_input_0_):
                    """
                    par_release_input_0_: (Double,Double)
                    """
                    def anonymous_fun_48_(orange_input_8_):
                      """
                      orange_input_8_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_8_)
                    def anonymous_fun_49_(orange_input_9_):
                      """
                      orange_input_9_: Double
                      """
                      return laplace_fx(cfix(1.0),orange_input_9_)
                    return (anonymous_fun_48_(par_release_input_0_[0]),anonymous_fun_49_(par_release_input_0_[1]))
                  return (anonymous_fun_46_(par_release_input_1_[0]),anonymous_fun_47_(par_release_input_1_[1]))
                return (anonymous_fun_44_(par_release_input_2_[0]),anonymous_fun_45_(par_release_input_2_[1]))
              return (anonymous_fun_42_(par_release_input_3_[0]),anonymous_fun_43_(par_release_input_3_[1]))
            return (anonymous_fun_40_(par_release_input_4_[0]),anonymous_fun_41_(par_release_input_4_[1]))
          return (anonymous_fun_38_(par_release_input_5_[0]),anonymous_fun_39_(par_release_input_5_[1]))
        return (anonymous_fun_36_(par_release_input_6_[0]),anonymous_fun_37_(par_release_input_6_[1]))
      return (anonymous_fun_34_(par_release_input_7_[0]),anonymous_fun_35_(par_release_input_7_[1]))
    return (anonymous_fun_32_(par_release_input_8_[0]),anonymous_fun_33_(par_release_input_8_[1]))
  return anonymous_fun_31_
fused_vector_tup_0_ = bmcs(10,[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0],(),anonymous_fun_0_,(),anonymous_fun_30_)
[fused_vector_tup_0_[0],(fused_vector_tup_0_[1])[0],((fused_vector_tup_0_[1])[1])[0],(((fused_vector_tup_0_[1])[1])[1])[0],((((fused_vector_tup_0_[1])[1])[1])[1])[0],(((((fused_vector_tup_0_[1])[1])[1])[1])[1])[0],((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[0],(((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[0],((((((((fused_vector_tup_0_[1])[1])[1])[1])[1])[1])[1])[1])[1]]