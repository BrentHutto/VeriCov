# VeriCov
Functions for fsQCA consistency and coverage measures and plots, as published by F. Veri (2018, 2019)
Over the past two years, Francisco Veri has published articles proposing new ways of examining coverage and consistency of fsQCA models.
In his 2018 paper *Coverage in Fuzzy Set Qualitative Comparative Analysis (fsQCA): A New Fuzzy Proposition for Describing Empirical Relevance*
suggested re-conceptualizing "coverage" as a construct with two dimensions, qualitative and quantitative. In 2019, he published 
*Aggregation Bias and Ambivalent Cases: A New Parameter of Consistency to Understand the Significance of Set-theoretic Sufficiency in fsQCA*
which offered a possible way to assess the tendency of fsQCA models to create levels of apparent consistency even when faced with random data.

I wrote some functions to compute his suggested coverage and consistency measures, alongside the conventional ones originated by Ragin and 
the 2015 consistency measure due T. Haesebrouck. I also duplicated some of the ways of plotting "ambiguous" versus "non-ambiguous" consistent
cases on XYplot, as used by Veri in his articles. 

This package contains the computation and plotting functions. The plots are basically done with a wrapper around Adrian Dusa's XYplot function
built in to the QCA package. I also included three example datasets referenced by the two Veri articles.
