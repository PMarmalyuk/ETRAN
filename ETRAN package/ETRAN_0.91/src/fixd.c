//============================================================
//
// Wrapper for LC Tecnologies I-DT algorithm
//============================================================
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <math.h>
#include  "fixfunc.h"

struct _stFIXdata
{
    float fixx;             //Fixation - x coordinate
    float fixy;				//Fixation - y coordinate
	float D;                //average pupil size
    int   duration;         //fixation duration (number of samples)
    int   start;            //fixation start (sample number)
	float fixdisp;          //fixation dispersion
};
//
//
//
SEXP FixDetectC(SEXP X, SEXP Y, SEXP R, SEXP Dispersion, SEXP MinDur, SEXP Debug)
{
    //function input
	double    *pDisp = REAL(Dispersion);
	double    *pMinDur = REAL(MinDur);
	int       fDebug; 
	//function output
	double *res = NULL;
	SEXP Res,r_X,r_Y,r_D,r_dur,r_start,r_disp;
	SEXP list_names;
	//detection parameters =================================================
	int    MinDuration;          //min dispersion duration (samples)
	float  Threshold;            //dispersion threshold
	float  Xi,Yi;				 //X & Y coordinates
	int    bGaze;                //valid data flag
	//LC detector output =============================================================	
	float  X_d,Y_d,FixX,FixY;
    float  Deviation_d;
	int    SaccadeDuration,FixDuration;
	int    bGaze_d;
	int    rc;                 //return code
	//=================================
    char *names[6] = {"X","Y","R","Dur","Start","Disp"};		
	int i,len,lmax,iend,istart;
	float  Ri, dMean;      //current and average pupil size
	int j, Dcnt;           //after-detection pupil size and dispersion calculation
	int MaxNFix, NFix;
	struct _stFIXdata *pFIX_b, *pFIX;
	size_t FIXdata_sz;
	float fDx,fDy,dDrSq,fD,fDisp;

	fDebug = INTEGER_VALUE(Debug);              //Flag - debug	
	MinDuration = (int) *pMinDur;
	Threshold = (float) *pDisp;
	len=LENGTH(X);
	lmax=len - 1;
	i=0;

	MaxNFix = len / MinDuration;
	if (fDebug > 0) Rprintf("FixDetectC start: len =%d MaxNFix=%d \n",len,MaxNFix);	
	FIXdata_sz = MaxNFix*sizeof(struct _stFIXdata);
	pFIX_b = (struct _stFIXdata *) malloc(FIXdata_sz);
	if (pFIX_b == NULL) {
	  Rprintf("FixDetectC: memory allocation ERROR (fixations BLOCK allocation)\n");
	  PROTECT(Res = allocVector(REALSXP, 1));
	  res = REAL(Res);
	  *res = 0;
	  UNPROTECT(1);
	  return Res;	  
	}
	
	pFIX = pFIX_b;
	NFix=0; 
		
	if (fDebug > 0) Rprintf("FixDetectC: InitFication Call\n");
	InitFixation(MinDuration);
	if (fDebug > 0) Rprintf("FixDetectC:  main while(i < len) \n");
	while (i < len) {
	   Xi = (float) REAL(X)[i];
	   Yi = (float) REAL(Y)[i];
	   Ri = (float) REAL(R)[i];
	   
	   if (Ri > 0) bGaze = 1;  else bGaze = 0;
	   
	   rc=DetectFixation(bGaze,Xi,Yi,Threshold,MinDuration,&bGaze_d,&X_d,&Y_d,&Deviation_d,&FixX,&FixY,&SaccadeDuration,&FixDuration);
	   if (fDebug > 1) Rprintf("FixDetectC DetectFixation %d,%d,%4.2f,%4.2f\n",i,rc,Xi,Yi);

       if ((rc == 2)||((rc == 1)&&(i == lmax)) ) {
                //---  Fixation finished -----
                iend   = i - MinDuration;
                istart = iend - FixDuration + 1;
				// Вычисляем средний диаметр зрачка
				j = 0; Dcnt=0; dMean=0; fD = 0; fDisp=0;
				for (j = istart; j < iend; j++) {
				   Ri = (float) REAL(R)[j];
                   Xi = (float) REAL(X)[j];
                   Yi = (float) REAL(Y)[j];				   
                   if (Ri > 0) { Dcnt++; dMean += Ri; }	
				   fDx = FixX - Xi;
                   fDy = FixY - Yi;
                   dDrSq = fDx * fDx + fDy * fDy;
                   fD = fD + dDrSq;
				   
				}
				if (Dcnt >0) dMean = dMean / Dcnt;
				if (Dcnt > 1) fDisp = (float)sqrt(fD / (Dcnt - 1));

				// Выводим информацию о фиксации 
				if (fDebug > 0) Rprintf("FixDetectC fixation: %4.2f,%4.2f,%3.5f,%d,%d\n",FixX,FixY,dMean,istart,FixDuration);
				if (FixDuration >= MinDuration) {
				   pFIX->fixx     = FixX;
				   pFIX->fixy     = FixY;
				   pFIX->D        = dMean;
				   pFIX->duration = FixDuration;
				   pFIX->start    = istart;
				   pFIX->fixdisp  = fDisp;
				   if ( NFix < MaxNFix) {pFIX++; NFix++;}
				   else {
				        Rprintf("FixDetectC: ERROR NFix (%d) == MaxNFix (%d)",NFix,MaxNFix);
						break;
					}
				}
				
        }
        i++; 
	}
	
	if (fDebug > 1) Rprintf("FixDetectC main loop finished\n");	
	
	PROTECT(Res = allocVector(VECSXP,6));
	PROTECT(r_X     = allocVector(REALSXP,NFix));
	PROTECT(r_Y     = allocVector(REALSXP,NFix));
	PROTECT(r_D     = allocVector(REALSXP,NFix));
	PROTECT(r_dur   = allocVector(INTSXP,NFix));
	PROTECT(r_start = allocVector(INTSXP,NFix));
	PROTECT(r_disp  = allocVector(REALSXP,NFix));	
	PROTECT(list_names = allocVector(STRSXP,6));	
	//=======
	pFIX = pFIX_b;
	i=0;
	while (i<NFix) {
		REAL(r_X)[i]        = pFIX->fixx;
		REAL(r_Y)[i]        = pFIX->fixy;
		REAL(r_D)[i]        = pFIX->D;
		INTEGER(r_dur)[i]   = pFIX->duration;
		INTEGER(r_start)[i] = pFIX->start;
		REAL(r_disp)[i]     = pFIX->fixdisp;
		pFIX++; i++;
	}

	SET_VECTOR_ELT(Res,0,r_X);		
	SET_VECTOR_ELT(Res,1,r_Y);
	SET_VECTOR_ELT(Res,2,r_D);
	SET_VECTOR_ELT(Res,3,r_dur);
	SET_VECTOR_ELT(Res,4,r_start);	
	SET_VECTOR_ELT(Res,5,r_disp);		
	//== Set names for output LIST elements  ===========================		 
    for(i = 0; i < 6; i++)  SET_STRING_ELT(list_names,i,mkChar(names[i])); 
    setAttrib(Res, R_NamesSymbol, list_names); 	
	UNPROTECT(8);
	free(pFIX_b);
	return Res;
}
