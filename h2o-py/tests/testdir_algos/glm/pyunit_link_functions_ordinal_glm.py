from __future__ import division
from __future__ import print_function
import sys
sys.path.insert(1,"../../../")
import h2o
from tests import pyunit_utils
from h2o.estimators.glm import H2OGeneralizedLinearEstimator

def link_functions_binomial():

  h2o_df = h2o.import_file(path=pyunit_utils.locate("smalldata/iris/iris.csv"))
  multinomial_fit = H2OGeneralizedLinearEstimator(family = "ordinal", solver="COORDINATE_"
                                                                             "DESCENT")
  multinomial_fit.train(y = 4, x = [0,1,2,3], training_frame = h2o_df)
  print("Done now")



if __name__ == "__main__":
  pyunit_utils.standalone_test(link_functions_binomial)
else:
  link_functions_binomial()
