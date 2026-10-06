# space-debris
co-evolutionary model of space-debris mitigation
Here we provide the code to reproduce the figures contained in manuscript titled "Innovate and cooperate to escape a tragedy of orbital commons" by Jorge M. Pacheco, Francisco C. Santos and Simon A. Levin
The code is a combination of 
1. Fortran files & bash scripts to perform an exhaustive search in parameter space, which allows us to extract the "Most Prevalent Configurations" (see manuscript for details)
2. Mathematica notebooks which implement the model (Space-Debris-Model.nb), integrate the co-evolutionary equations in phase space and in time, and generate figures, from Fig.-01 to Fig-04, as well as Extended Data figures: ED-fig-02, ED-Fig-03 and ED-Fig-04. 
3. The remaining figures are generated via excel compatible worksheets with self-explanatory naming. 
All fortran code was compiled using gffortran on a linux intel X64 workstation (_Make scripts available), and all mathematica notebooks were coded and exceuted using Mathematica 13.3. 
The Space-Debris-Model.nb employes a fortran code (mathconvert.f, _Make cript available) which interacts with the mathematica notebook for input-output. 
Finally, different notebooks may require data stored in sub-directories also made available. 

Upon excecution of bash script RUNALL, the exhaustive search in parameter space will lead to the data files (already available) hist-s-1.dat to hist-s-4.dat
From these files, the "Most Prevalent Configurations" are stored in files config.inp.1 to config.inp.4, necessary to run the different mathematica notebooks. 
NOTE: the fortran code may generate many GB of data. 
