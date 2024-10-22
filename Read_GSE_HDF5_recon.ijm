run("HDF5...", "Variable Name Selection=/exchange/data/");
run("Duplicate...", "duplicate");
run("XOR...", "value=1000000000000000 stack");
run("Calibrate...", "function=[Straight Line] unit=[Gray Value] text1=[0 65535] text2=[-32768 32767 ]");
run("Enhance Contrast", "saturated=0.5");
close("*h5 data");
