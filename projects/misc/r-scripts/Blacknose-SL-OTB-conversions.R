# Converting Length to Weight for Blacknose Shark using Blue Shark Coefficients

# SL to OTB
# Using combined Blue Shark coefficients
FL.sl = 85.3
otb.a = -0.9004
otb.b =  0.9803
FL.otb = (FL.sl-otb.a)/otb.b
FL.otb
  
# OTB to Weight
weight.a = 2.52e-6
weight.b = 3.32
W = weight.a*FL.otb^weight.b
W

# OTB to dressed
dressed = W/1.39
dressed

data.frame(`Straight Fork Length` = FL.sl,
           `OTB Fork length` = FL.otb,
           `Weight` = W,
           `Dressed Weight` = dressed,
           check.names = FALSE)
