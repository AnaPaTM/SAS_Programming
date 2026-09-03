%*********************************************************************;
%**	Program: MatchingBasedReg_scenario_3-3_3-2.sas
%**
%**	Project:  Matching Techniques - for NEMS
%**			
%**	Objective: Parte of a Monte Carlo Simulation
%**
%**	Author: Ana Paula Miranda
%**
%**	Date created: Long time ago.
%**
%**	Date Modifiend:
%**
%**	Comments: Please see the beauty of this program and coding techniques. Enjoy!
%**********************************************************************;

Options Obs=Max Ls=Max Ps=max Nocenter Mprint NoXWait Compress=yes Fullstimer Nomtrace Nomlogic Formchar="|----|+|---+=|-/\<>*" 
		Sasautos=("G:\EHD\Magnet Conversion Project 2006\AnaWorkDir_Analysis\Programs" sasautos);


%macro MatchingStudy (Inpath=, Outpath=, DSN=, StartYr=, EndYr=, StartLoop=, N_loops=1, Subject=, Scenario=);


	Libname In    "&InPath.";
	Libname Out   "&OutPath.";
	

	%do Loop=&StartLoop. %to &N_loops.;
	
		proc printto   log="&OutPath.\logs\&subject._LastLoop_MtchdBased_&Scenario..log" 
		             print="&OutPath.\logs\&subject._LastLoop_MtchdBased_&Scenario..lst" NEW;
		run;
			
		%Put "###" Loop=&Loop. "###" ;
			
		proc sql noprint;
			select unique LEAID into :Dst1 - :Dst999999 from In.&DSN.;
			select count(unique LEAID) into :n_Dst from In.&DSN.;
		quit;
		
		
		%do D=1 %to  &n_Dst.; %let District=&&Dst&D;

			data _&District._;
				Set In.&DSN. (where=(Upcase(LEAID)="%Upcase(&District.)"));
				
				sch_&subject._3_2_diff = (sch_&subject._3 - sch_&subject._2);
				sch_&subject._4_3_diff = (sch_&subject._4 - sch_&subject._3);
			run;
			proc sort data=_&District._; by NCESSCH; run;
			
			
			proc reg data=&Syslast. 	OutEst=Coefficients;				
				Title5 "******* MATCHING Regression: District = &District. and Loop=&Loop ********";				
				model sch_&subject._3_2_diff = sch_&subject._1  sch_&subject._2  PerfRL02  PerBlack02  PerHisp02;
				output out=XBeta  P=Xbeta_change_3_2;
			run;
			quit;


			data Predicted_0304;
				set XBeta;
				if _n_=1 then set Coefficients (keep=Intercept  sch_&subject._1  sch_&subject._2  PerfRL02  PerBlack02  PerHisp02
								  				rename=(sch_&subject._1=Coeff_sch_&subject._1  sch_&subject._2=Coeff_sch_&subject._2  
								  		  				PerfRL02=Coeff_PerfRL02                PerBlack02=Coeff_PerBlack02  
								  		  				PerHisp02=Coeff_PerHisp02)
								  		  	   );

				if Xbeta_change_3_2^=.
					then Xbeta_change_4_3 = Sum(Intercept,  Coeff_sch_&subject._1 * sch_&subject._2,
															Coeff_sch_&subject._2 * sch_&subject._3,
															Coeff_PerfRL02        * PerfRL03,
															Coeff_PerBlack02      * PerBlack03,
															Coeff_PerHisp02       * PerHisp03);						
				
				if rand_mag&loop.=1 then do;
					call symputx("XBeta_Magnet", Xbeta_change_4_3);
				end;
			run;
			proc sort data=Predicted_0304; by Xbeta_change_4_3;
			
			proc sql noprint;
				select count(unique NCESSCH) into :NObs from Predicted_0304 where rand_mag&loop.^=1; 
			quit;			
				%put;
				%put NObs = &NObs.  MagnetXbetaValue = &XBeta_Magnet.;
				%put;
				
							
			data Matching (drop=rand_mag&loop. Xbeta_change_4_3 School_RowID NCESSCH) CompSchools (keep=School_RowID);
				set Predicted_0304 (keep=NCESSCH rand_mag&loop. Xbeta_change_4_3 where=(rand_mag&loop.^=1)) End=Eof;
				by Xbeta_change_4_3;

				array _XBeta(&NObs.);

				retain  _XBeta NCombinations;

				NCombinations = (Fact(&NObs.)/(Fact(&NObs.-2)*2));***for checks only;
				Record_Number+1;           			           			
				_XBeta_Magnet = &XBeta_Magnet.;
				***JULIAN FIX1 - abs function was moved from here to the mathing step below instead;
				_XBeta{Record_Number}=(Xbeta_change_4_3 - _XBeta_Magnet);					

				School_RowID = Catx("-", NCESSCH, Record_Number); 

				if Eof then do;											

					Put NCombinations=;

					%do I=1 %to &Nobs.;

						%let J=&Nobs.;

						%do %while(&J>&I);

							MeanBtwnRecs_&I._&J.=abs(((_XBeta&I. + _XBeta&J.)/2));

							%let J = %Eval(&J. - 1);

						%end;

					%end;

					Minimum_Diff_Value = Min(of MeanBtwnRecs_:);

					output Matching ;							

				end;

				output CompSchools;
			run;

			*** the minimum diff var is kept just for the purpose of confirming the results;
			proc transpose data=Matching (keep=MeanBtwnRecs_:  Minimum_Diff_Value) out=Selection Name=School_Matching Prefix=XBeta; run;				
			proc sort data=Selection; by XBeta1; run;

			data _null_;
				set Selection;

				if _n_=1 then do;
					call symputx("Match_Found_Sch1", Scan(School_Matching, -2, "_"));
					call symputx("Match_Found_Sch2", Scan(School_Matching, -1, "_"));
				end;
			run;

			%put "###" Comparison School One= &Match_Found_Sch1.  Comparison School Two= &Match_Found_Sch2. "###";
			%put;

			proc sql noprint;
				select Compress(Scan(School_RowID, 1, "-")) into :Control_ID1 from CompSchools where Scan(School_RowID, -1, "-")="&Match_Found_Sch1.";
				select Compress(Scan(School_RowID, 1, "-")) into :Control_ID2 from CompSchools where Scan(School_RowID, -1, "-")="&Match_Found_Sch2.";
			quit;

			*** The 2 closest matches are:;
			%put "***" final_school_selectionId_1=&Control_ID1  final_school_selectionId_2=&Control_ID2 "***";
			%put;

			**Creating final District matched data, which consists of the 2 selected control schools and the hyp magnet school, nobs=3;
			data &Subject._D&District.; 
				set Predicted_0304;

				Comparison&loop.=(NCESSCH="&Control_ID1." or NCESSCH="&Control_ID2.");

				if rand_mag&loop.=1 or Comparison&loop.=1 then output;
			run;
		
		***ending district loop;
		%end;

		***Stacking all Districts back together, consisting of the hypothetical magnet and 2 comparison schools each;
		data &Subject._Loop&Loop._Matched;
			Set %do D=1 %to &n_Dst.; %let District=&&Dst&D; &Subject._D&District. %end;;
		run;	
								
		proc sql noprint;
			select count(unique LEAID) into :N_Districts from &Subject._Loop&Loop._Matched;
			select count(unique NCESSCH) into :N_Schools from &Subject._Loop&Loop._Matched;
		quit;
		
		***Creating the LONG file ***;
		%do Year=&startyr. %to &endyr.;
			data Year_&Year.;
				set &Subject._Loop&Loop._Matched;;

				 **JULIAN FIX2;	 
				 **coding a new hypothetical magnet status dummy to no status during the first 3 years of data;
				 **the magnet status will only start showing at year 4 and forward;
				 %if &Year.=1 %then rand_mag&loop._fake=0;;
				 %if &year.=2 %then rand_mag&loop._fake=0;;
				 %if &year.=3 %then rand_mag&loop._fake=0;;
				 %if &Year.=4 %then rand_mag&loop._fake=rand_mag&loop.;;
				 %if &year.=5 %then rand_mag&loop._fake=rand_mag&loop.;;
				 %if &year.=6 %then rand_mag&loop._fake=rand_mag&loop.;;					 

				Keep LEAID NCESSCH 
					 sch_&subject._&year.  
					 level0&year.
					 magnet0&year.
					 member0&year.
					 perfrl0&year.
					 perhisp0&year.
					 perblack0&year.  
					 rand_mag&loop.  
					 rand_mag&loop._fake
					 Comparison&loop.;			
			run;
			proc sort data=Year_&Year.; by LEAID NCESSCH; run;			
		%end;		
		
		
		data &Subject._Loop&Loop._MatchedLong;
			set %do Year=&startyr. %to &endyr.; 
					Year_&Year.	(in=Data_&Year. rename=(sch_&subject._&year.=sch_&subject.  
													 level0&year.=level
													 magnet0&year.=magnet
													 member0&year.=member
					                                 PERFRL0&Year.=perfrl           
					                                 PERHISP0&Year.=perhisp  
					                                 PERBLACK0&Year.=perblack)
					             ) 
				%end;;
			by LEAID NCESSCH;
			
			***Building the fixed effects dummies***;
			**1) Year;
			%do Year=&startyr. %to &endyr.;
				Year_0&Year.=(Data_&Year.=1);
			%end;
			
			array NCESSCH_(&N_Schools.);
			array DIST_(&N_Districts.);			
			
			retain NCESSCH_  DIST_;
			
			**2) Schools;
			if first.NCESSCH then do;
				%do I=1 %to &N_Schools.; NCESSCH_&I.=0; %end;
				n_school+1;
			end;			
			NCESSCH_(n_school)=1;			
			
			**Districts;
			if first.LEAID then do;
				%do J=1 %to &N_Districts.; DIST_&J.=0; %end;
				n_dist+1;
			end;			
			DIST_(n_dist)=1;
			
			**3) Districts*Year interactions;
			%do J=2 %to &N_Districts.;
				%do Year=%eval(&startyr.+1) %to &endyr.;
					DistYear_&J.&Year.= (DIST_&J.*Year_0&Year.);
				%end;
			%end;				
		run;


		*** Finally the - OUTCOME - regression ***;
		*** leaving out the first school and year dummies;
		/*proc reg data=&Subject._Loop&Loop._MatchedLong;
			Title5 "******* OUTCOME Regression: Loop=&Loop ********";			
			model sch_&subject. = PERFRL  PERHISP  PERBLACK  rand_mag&loop._fake 
			                      Year_0%eval(&StartYr+1)-Year_0&EndYr 
			                      %do I=2 %to &N_Schools.; NCESSCH_&I. %end; 
			                      DistYear_:
			;			
			ODS output ParameterEstimates=ParameterEsts_&subject._loop&loop.;
		run;*/

		proc genmod data=&Subject._Loop&Loop._MatchedLong;
			Title5 "******* OUTCOME Regression: Loop=&Loop ********";			
			class LEAID;
			model sch_&subject. = PERFRL  PERHISP  PERBLACK  rand_mag&loop._fake  
								  Year_0%eval(&StartYr+1)-Year_0&EndYr. 
			                      %do I=2 %to &N_Schools.; NCESSCH_&I. %end;  
			                      %do J=2 %to &N_Districts.;
									%do Year=%eval(&StartYr.+1) %to &Endyr.;
										DistYear_&J.&Year.
									%end;
								%end;
			;
			repeated subject = LEAID;
			
			ODS output GEEEmpPEst=ParameterEsts_&subject._loop&loop.;
		run; 
		/*JULIAN FIX3: For the YEAR*DISTRICT interaction dummies, I've applied the following fix "... To fix this, leave the above year dummies 
		  in to control for trends in the first district, and then add interactions between the year 2 through last year dummies and dummies 
		  for each of second through nth districts." */ 

				
	***ending loop;
	 %end;
	 
	 
	***Saving all parameters from loop1-whatever from the Hyp Magnet in a single file;
	 data out.Parameters_&Subject._&N_loops.x;
	 	length Parm $50;
	 	set %do Loop=&StartLoop. %to &N_loops.; ParameterEsts_&subject._loop&loop. (where=(Parm="rand_mag&loop._fake")) %end;; 
	 	
	 	Loop+1;
	 		
	 	NoSignif_Hyp_Rejected=(ProbZ<=0.05);	 	
	 	
	 	Model_N_Years_Included="&StartYr.-&EndYr.";
	 	Scenario="&Scenario.";
	 run;

	proc sql;
		create table out.MatchingEff_&Subject._&N_loops.x as select 
			Sum(NoSignif_Hyp_Rejected)/&N_loops. as Pct_Sig, 
			Mean(Estimate) as Estimate_Mean,
			STD(Estimate) as Estimate_MSE from out.Parameters_&Subject._&N_loops.x;
	quit;
		
	
	PROC DATASETS LIB=work KILL;
	RUN;
		
	 
%MEnd; 
*** FOR 3-3;
%MatchingStudy (Inpath= ..., 
                Outpath= ..., 
                DSN=  ...
		);                        
*** FOR 3-2;
%MatchingStudy (Inpath= ..., 
                Outpath= ..., 
                DSN= ...);