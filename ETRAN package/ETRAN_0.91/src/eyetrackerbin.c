#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#define BUF_LEN 600		        	//max string (for file reading) length
#define MAX_N_FIELDS 60             //max number of data fields to read
//
// Full data set from SMI file (converted with IDF Converter) 
// 
//
struct _stSMIeye{
    char    m_InFileName[200];      //Input file name
    FILE    *m_InFile;              //Input file descriptor	
	int     m_ReadRAW;              //Flag - read raw data
	int     m_ReadEPOS;             //Flag - read EPOS & GVEC data from RED-m	
	int     m_ReadTrial;            //Flag - read Trial data
	int     m_Debug;                //Flag - debug
    int     m_SampleRate;           //System sampling Rate	
	int     m_AreaW;				//Area width (pixels)
	int     m_AreaH;				//Area height (pixels)
    int     m_AreaW_mm;				//Area width (mm)
    int     m_AreaH_mm;				//Area height (mm)
	int     m_HeadDist_mm;          //Distance to head (mm)
    int     m_NRecords;             //Number of SMP records in data file	
	int     m_NMsgRecords;			//Number of MSG records in data file
    int     m_NActualRecords;       //Number of processed SMP records	
	double  m_databuf[MAX_N_FIELDS];//Data buffer for scanSMIline()	
    char    m_CrntMsgTxt[100];      //Current MSG text
	double  m_CrntMsgTime;          //Current MSG time
	long long int m_TimeStart;		//Start time (mks)
	long long int m_TimeCrnt;       //Current time (mks)
	char    m_SubjectName[100];     //Test subject name
    int     m_CurrentFrame;         //Current frame number    
	int     n_Time;					//Number of "Time" fields                         (0 - 1)
	int     n_Trial;                //Number of Trial fields                          (0 - 1)
	int     n_DIAl;                 //Number of "DIA" (eye diameter) fields left      (0 - 2)
	int     n_DIAr;                 //Number of "DIA" (eye diameter) fields right     (0 - 2)	
	int     n_mDIAl;                //Number of Mapped Diameter [mm] fields           (0 - 1)
	int     n_mDIAr;                //Number of Mapped Diameter [mm] fields           (0 - 1)	
	int     n_PORl;                 //Number of "POR" (point of regard) fields  left  (0 - 2)	
	int     n_PORr;                 //Number of "POR" (point of regard) fields  right (0 - 2)		
    int     n_RAWl;                 //Number of "RAW" (pupil center) fields left      (0 - 2)
    int     n_RAWr;                 //Number of "RAW" (pupil center) fields right     (0 - 2)
	int     n_CRl;                  //Number of "CR" (corneal reflex) fields left     (0 - 1 - 2)
	int     n_CRr;                  //Number of "CR" (corneal reflex) fields right    (0 - 1 - 2)
	int     n_EPOSl;                //Number of "EPOS" (absolute eye position) fields left (0 - 3)
	int     n_EPOSr;                //Number of "EPOS" (absolute eye position) fields right(0 - 3)
	int     n_GVECl;                //Number of "GVEC" (gaze vector) fields left      (0 - 3)
	int     n_GVECr;                //Number of "GVEC" (gaze vector) fields left      (0 - 3)
	int     poz_Time;               //Index of 1st "Time" field
	int     poz_Trial;              //Index of Trial field
	int     poz_mDIAl;              //Index of left mapped diameter field
	int     poz_mDIAr;              //Index of right mapped diameter field	
	int     poz_DIAl;               //Index of left  "DIA"  field
	int     poz_DIAr;               //Index of right "DIA"  field	
	int     poz_PORl;               //Index of left  "POR"  field
	int     poz_PORr;               //Index of right "POR"  field
	int     poz_RAWl;               //Index of left  "RAW"  field
	int     poz_RAWr;               //Index of right "RAW"  field
	
	int     poz_CRl;                //Index of left  "CR"   field
	int     poz_CRr;                //Index of right "CR"   field
	
	int     poz_EPOSl;              //Index of 1st "EPOS" field
	int     poz_EPOSr;              //Index of 1st "EPOS" field	
	int     poz_GVECl;              //Index of 1st "GVEC" field
	int     poz_GVECr;              //Index of 1st "GVEC" field	
	int     n_total_datafields;     //Total number of fields to read    
//=========================================================================	
    int     m_DataType;             //0 - данные отсутствуют
                                    //1 - загружен csv-файл,  экспортированный из IDF
	int		m_binocular;			//0 - монокул€рный формат (заполн€ютс€ записи только дл€ левого глаза)
									//1 - бинокул€рный формат
	int     m_Raw;                  //0 - "сырые" данные отсутствуют; 1 - RAW + CR1; 2 - RAW+CR1+CR2
	int     m_EyePosition;          //0 - данные отсутствуют; 1- данные с REDm об абсолютной позиции и направлении взора
	int     d_idx ;                 //index in data buffer
};
//
// Check current string type:  SMP type - 0; MSG type - 2; ERROR - 1;
//
int IsSMP(char *buff)
{
    char *p;
	int i;
    p = buff;
    if ((*p < '0')||(*p > '9')) return 1;
	i=0;
    while (((*p >= '0')&&(*p <= '9'))||(*p == 0x09)) {
		p++;
		i++;
		if (i > 12) break;
	}
    if (strncmp(p,"SMP",3)==0) return 0;
	if (strncmp(p,"MSG",3)==0) return 2;
    return 1;
}
//
// Scan current string for number
//
char* Scan4Number (char *pin, char *pout, int ScanMax)
{
   int i;
   i=0; 
   while (((*pin >= '0')&&(*pin <= '9'))||(*pin == '.')||(*pin == '-')) {
      *pout++ = *pin++;
	  if (i++ > ScanMax) break;
	}
	*pout=0x00; if (*pin != 0x00) pin++;
	return(pin);
}	
//
// Scan current string for substring
//
char* Scan4String (char *pin, char *pout, int ScanMax)
{
   int i;
   i=0; 
   while ((*pin != 0x0d)&&(*pin != 0x0a)&&(*pin != 0x00)&&(*pin != 0x20)) {
      *pout++ = *pin++;
	  if (i++ > ScanMax) break;
	}
	*pout=0x00; if (*pin != 0x00) pin++;
	return(pin);
}
//
//
// Scan current string for substring with spaces
//
char* Scan4StringWithSP (char *pin, char *pout, int ScanMax)
{
   int i;
   i=0; 
   while ((*pin != 0x0d)&&(*pin != 0x0a)&&(*pin != 0x00)) {
      *pout++ = *pin++;
	  if (i++ > ScanMax) break;
	}
	*pout=0x00; if (*pin != 0x00) pin++;
	return(pin);
}
//
// Scan current string for field (finished with 0x09)
//
char* Scan4Field (char *pin, char *pout, int ScanMax)
{
   int i;
   i=0; 
   while ((*pin != 0x0d)&&(*pin != 0x0a)&&(*pin != 0x00)&&(*pin != 0x09)) {
      *pout++ = *pin++;
	  if (i++ > ScanMax) break;
	}
	*pout=0x00; if (*pin != 0x00) pin++;
	return(pin);
}
//
// Go to begin of specified field (finished with 0x09) in string 
//
char* GoToField(char *pin,int n)
{
   int i;
   i=1; 
   while(1) {
     if (*pin == 0x09) i++;
	 if (i == n) break;
	 pin++;
   }
   pin++;
   return(pin);
}
//
void GetFields(struct _stSMIeye *EyeD,int Poz, int Nf,char *buff)
{
   char *p;
   char work[50];
   int i;
   if (Nf > 0) {
      p = GoToField(buff,Poz);
	  for (i=0; i< Nf; i++) {  
	        EyeD->d_idx++;  
		    p=Scan4Number(p,work,15); 
			EyeD->m_databuf[EyeD->d_idx]=atof(work); 
	  }
   }
}
//
// Process next string from data file
// 0 - SMP line
// 1 - MSG line
// 2 - line don't starte with number (comment?)
// 3 - EOF
// 4 - ERROR (in MSG line)
// 5 - ERROR (Type field)
// 6 - ERROR (in SMP line)
//
int ScanSMIline(struct _stSMIeye *EyeD)
{
	char *p,*p2,buff[BUF_LEN],work[25];
	int i,n;
	
	p=fgets(buff,BUF_LEN,EyeD->m_InFile);	
    if (p == NULL) return(3);      //Return: end of file
	p=Scan4Number(buff,work,15);
	if (work[0] == 0x00) return(2); //Line don't started with number - skip
	//Line start with number -- get this number as Time field
	//Rprintf("ScanSMIline work=%s||\n",work);
	//Rprintf("%c%c%c%c||\n",*p,*(p+1),*(p+2),*(p+3));
	EyeD->m_TimeCrnt = strtoll(work,&p2,10);
    if (EyeD->m_TimeStart == 0) EyeD->m_TimeStart = EyeD->m_TimeCrnt;
	//Check 2nd field (line type)
	if (strncmp(p,"SMP",3) == 0) {
	  EyeD->d_idx=0;	
	  EyeD->m_databuf[EyeD->d_idx]=(double)((EyeD->m_TimeCrnt - EyeD->m_TimeStart)/1000.0);
	  GetFields(EyeD,EyeD->poz_Trial,EyeD->n_Trial,buff);
	  GetFields(EyeD,EyeD->poz_PORl,EyeD->n_PORl,buff);
	  GetFields(EyeD,EyeD->poz_PORr,EyeD->n_PORr,buff);	  
	  GetFields(EyeD,EyeD->poz_DIAl,EyeD->n_DIAl,buff);	  
  	  GetFields(EyeD,EyeD->poz_DIAr,EyeD->n_DIAr,buff);	 
	  GetFields(EyeD,EyeD->poz_mDIAl,EyeD->n_mDIAl,buff);	  
  	  GetFields(EyeD,EyeD->poz_mDIAr,EyeD->n_mDIAr,buff);	 	  
	  GetFields(EyeD,EyeD->poz_RAWl,EyeD->n_RAWl,buff);	  
  	  GetFields(EyeD,EyeD->poz_RAWr,EyeD->n_RAWr,buff);	 
  	  GetFields(EyeD,EyeD->poz_CRl,EyeD->n_CRl,buff);	  
  	  GetFields(EyeD,EyeD->poz_CRr,EyeD->n_CRr,buff);	 
  	  GetFields(EyeD,EyeD->poz_EPOSl,EyeD->n_EPOSl,buff);
  	  GetFields(EyeD,EyeD->poz_EPOSr,EyeD->n_EPOSr,buff);
  	  GetFields(EyeD,EyeD->poz_GVECl,EyeD->n_GVECl,buff);
  	  GetFields(EyeD,EyeD->poz_GVECr,EyeD->n_GVECr,buff);
      //==================================================
	  EyeD->d_idx++; n=EyeD->n_total_datafields - 1;
	  if (EyeD->d_idx != n) {
	      Rprintf("ScanSMIline ERROR(6) in line %s\n",buff);	  
		  Rprintf("Read totaly: %d Expected: %d\n",EyeD->d_idx,n);
		  Rprintf("PORl: %d %d\n",EyeD->poz_PORl,EyeD->n_PORl);
		  Rprintf("PORr: %d %d\n",EyeD->poz_PORr,EyeD->n_PORr);	
		  Rprintf("DIAl: %d %d\n",EyeD->poz_DIAl,EyeD->n_DIAl);	
		  Rprintf("DIAr: %d %d\n",EyeD->poz_DIAr,EyeD->n_DIAr);	
		  Rprintf("EPOSl: %d %d\n",EyeD->poz_EPOSl,EyeD->n_EPOSl);	
		  Rprintf("EPOSr: %d %d\n",EyeD->poz_EPOSr,EyeD->n_EPOSr);
		  Rprintf("GVECl: %d %d\n",EyeD->poz_GVECl,EyeD->n_GVECl);	
		  Rprintf("GVECr: %d %d\n",EyeD->poz_GVECr,EyeD->n_GVECr);			  
		  for (i=0; i<EyeD->d_idx; i++) {
		    		  Rprintf("Field[%d]=%f \n",i,EyeD->m_databuf[i]);
		  }
	      return(6); //Error - not correct number of data fields
	  }	  
	  EyeD->m_NActualRecords++;
	  return(0); //SMP string proceeding	  
	}  
    if (strncmp(p,"MSG",3) == 0) {
      p += 6;
      if (strncmp(p,"# Message:",10)==0) {
         p += 11; 
		 Scan4StringWithSP(p,EyeD->m_CrntMsgTxt,95);
		 EyeD->m_CrntMsgTime = (double)((EyeD->m_TimeCrnt - EyeD->m_TimeStart)/1000.0);
		 EyeD->m_CurrentFrame++;
         return (1); //MSG string proceeding
      }
      if (strncmp(p,"# UTC:",6)==0) {
         p += 2; 
		 Scan4StringWithSP(p,EyeD->m_CrntMsgTxt,95);
		 EyeD->m_CrntMsgTime = (double)((EyeD->m_TimeCrnt - EyeD->m_TimeStart)/1000.0);
		 EyeD->m_CurrentFrame++;
         return (1); //MSG string proceeding
      }
	  Rprintf("ScanSMIline ERROR(4) in line %s\n",buff);
  	  return(4); //Error - corrupted file ??
	}
	Rprintf("ScanSMIline ERROR(5) in line %s \n",p);	
	return(5); //Error - wrong type (2nd field)
}
//
// Get SMI data file structure 
//
int ReadIViewStruct(struct _stSMIeye *EyeD)
{
    char buff[BUF_LEN];	//Ѕуфер дл€ чтени€ строки
    char *p,*ps,work[30];
    int  i, rc,rc2;

    ps=fgets(buff,BUF_LEN,EyeD->m_InFile);
    if (strncmp(buff,"## [iView]",10)!=0) return 0;

    rc=0;
	EyeD->n_Time=0;		EyeD->poz_Time=0;   EyeD->n_Trial=0;        EyeD->poz_Trial=0;
	EyeD->n_DIAl=0;     EyeD->poz_DIAl=0; 	EyeD->n_DIAr=0;         EyeD->poz_DIAr=0;	
	EyeD->n_mDIAl=0;    EyeD->n_mDIAr=0;	EyeD->poz_mDIAl=0;      EyeD->poz_mDIAr=0;
	EyeD->n_PORl=0;     EyeD->poz_PORl=0;	EyeD->n_PORr=0;         EyeD->poz_PORr=0;	
	EyeD->n_RAWl=0;     EyeD->poz_RAWl=0;	EyeD->n_RAWl=0;         EyeD->n_RAWr=0;	
	EyeD->n_CRl=0;      EyeD->poz_CRl=0;	EyeD->n_CRr=0;          EyeD->poz_CRr=0;	
	EyeD->n_EPOSl=0;    EyeD->poz_EPOSl=0;	EyeD->n_EPOSr=0;        EyeD->poz_EPOSr=0;	
	EyeD->n_GVECl=0;    EyeD->poz_GVECl=0;	EyeD->n_GVECr=0;        EyeD->poz_GVECr=0;	
	EyeD->n_total_datafields=0;
    EyeD->m_NRecords=0;
	EyeD->m_NMsgRecords=0;	
	if (EyeD->m_Debug > 1) {
        Rprintf("ReadIViewStruct: initialization done\n");
	}	
    while(1) {
        ps=fgets(buff,BUF_LEN,EyeD->m_InFile); 
        if (ps == NULL) break;		                  //ѕроверка на конец файла†	
        if (rc < 4)	 {	
            if (strncmp(buff,"## Subject:",11)==0)        {Scan4String(buff+12,EyeD->m_SubjectName,100); rc++;}
		    if (strncmp(buff,"## Sample Rate:",15)==0)    {Scan4Number(buff+16,work,4); EyeD->m_SampleRate = atoi(work); rc++; }
		    if (strncmp(buff,"## Calibration Area:",20)==0){
		        p=Scan4Number(buff+21,work,5); 
				EyeD-> m_AreaW = atoi(work);
				Scan4Number(p,work,5);
				EyeD-> m_AreaH = atoi(work);
				rc++;
            }
			if (strncmp(buff,"## Stimulus Dimension [mm]:",27)==0) {
		        p=Scan4Number(buff+28,work,5); 
				EyeD-> m_AreaW_mm = atoi(work);
				Scan4Number(p,work,5);
				EyeD-> m_AreaH_mm = atoi(work);
            }
            if (strncmp(buff,"## Head Distance [mm]:",22) == 0) {
                  p=Scan4Number(buff+23,work,5); 
                  EyeD->m_HeadDist_mm = atoi(work);
            }				  
		    if (strncmp(buff,"Time",4)==0) {
			    if (EyeD->m_Debug > 1) {
                   Rprintf("ReadIViewStruct: description string is %s\n",buff);			
				}   
		        p=buff; i=0;
		        while(*p != 0x00) {
		            p=Scan4Field(p,work,20); i++;
					if (EyeD->m_Debug > 1) {
                        Rprintf("ReadIViewStruct: field is %s\n",work);						
					}

		            if (strcmp(work,"Time")==0)      {EyeD->n_Time++;  if (EyeD->poz_Time  == 0) EyeD->poz_Time=i;}
					if (EyeD->m_ReadTrial == 1) {					
					  if (strcmp(work,"Trial")==0)   {EyeD->n_Trial++; if (EyeD->poz_Trial == 0) EyeD->poz_Trial=i; 
					                                  if (EyeD->m_Debug > 0) {Rprintf("ReadIViewStruct: n_Trial =1\n");}
													 }
                      							  
					} 
		            if (strncmp(work,"L POR",5)==0)  {EyeD->n_PORl++;  if (EyeD->poz_PORl  == 0) EyeD->poz_PORl=i;}		
		            if (strncmp(work,"R POR",5)==0)  {EyeD->n_PORr++;  if (EyeD->poz_PORr  == 0) EyeD->poz_PORr=i;}						
		            if (strncmp(work,"L Dia",5)==0)  {EyeD->n_DIAl++;  if (EyeD->poz_DIAl  == 0) EyeD->poz_DIAl=i;}
		            if (strncmp(work,"R Dia",5)==0)  {EyeD->n_DIAr++;  if (EyeD->poz_DIAr  == 0) EyeD->poz_DIAr=i;}					
                    if (strncmp(work,"L Mapped Diameter",17)==0)  {  EyeD->n_mDIAl++;   if (EyeD->poz_mDIAl == 0) EyeD->poz_mDIAl=i;}
                    if (strncmp(work,"R Mapped Diameter",17)==0)  {  EyeD->n_mDIAr++;   if (EyeD->poz_mDIAr == 0) EyeD->poz_mDIAr=i;}					
                    //====================================================================================
					if (EyeD->m_ReadRAW == 1) {																					 
	                  if (strncmp(work,"L Raw",5)==0)  {EyeD->n_RAWl++;  if (EyeD->poz_RAWl  == 0) EyeD->poz_RAWl=i;}
	                  if (strncmp(work,"R Raw",5)==0)  {EyeD->n_RAWr++;  if (EyeD->poz_RAWr  == 0) EyeD->poz_RAWr=i;}					  
		              if (strncmp(work,"L CR",4)==0)   {EyeD->n_CRl++;   if (EyeD->poz_CRl   == 0) EyeD->poz_CRl=i;}	
					  if (strncmp(work,"R CR",4)==0)   {EyeD->n_CRr++;   if (EyeD->poz_CRr   == 0) EyeD->poz_CRr=i;}
					}
					if (EyeD->m_ReadEPOS == 1) {					
	                  if (strncmp(work,"L EPOS",6)==0) {EyeD->n_EPOSl++;  if (EyeD->poz_EPOSl  == 0) EyeD->poz_EPOSl=i;}
	                  if (strncmp(work,"R EPOS",6)==0) {EyeD->n_EPOSr++;  if (EyeD->poz_EPOSr  == 0) EyeD->poz_EPOSr=i;}						  
		              if (strncmp(work,"L GVEC",6)==0) {EyeD->n_GVECl++;  if (EyeD->poz_GVECl  == 0) EyeD->poz_GVECl=i;}
		              if (strncmp(work,"R GVEC",6)==0) {EyeD->n_GVECr++;  if (EyeD->poz_GVECr  == 0) EyeD->poz_GVECr=i;}						  
					}
		        }
		        rc++;
		        EyeD->n_total_datafields = EyeD->n_Time  + EyeD->n_RAWl + EyeD->n_RAWr + EyeD->n_CRl + EyeD->n_CRr +
                            			   EyeD->n_PORl  + EyeD->n_PORr + EyeD->n_DIAl + EyeD->n_DIAr + EyeD->n_mDIAl + EyeD->n_mDIAr + 
										   EyeD->n_EPOSl + EyeD->n_EPOSr + EyeD->n_GVECl + EyeD->n_GVECr +  EyeD->n_Trial + 1;
		    }	
		} else {
		  rc2 = IsSMP(buff);
          if (rc2 == 0)  EyeD->m_NRecords++;        //увеличиваем число строк SMP
		  if (rc2 == 2)  EyeD->m_NMsgRecords++;		//увеличиваем число строк MSG		 
		}
    }
	Rprintf("SubjName: %s\n",EyeD->m_SubjectName);
	Rprintf("Sample Rate: %d\n",EyeD->m_SampleRate);
    Rprintf("Area size: %d X %d\n",EyeD->m_AreaW,EyeD->m_AreaH);	
    Rprintf("File has %d SMP  and %d MSG records\n",EyeD->m_NRecords,EyeD->m_NMsgRecords);
	if (EyeD->m_Debug > 0) {	
	Rprintf("n_Time=%d,n_PORl=%d,n_PORr=%d,n_DIAl=%d,n_DIAr=%d,n_mDIAl=%d,n_mDIAr=%d,n_RAWl=%d,n_RAWr=%d,n_CRl=%d,n_CRr=%d,n_EPOSl=%d,n_EPOSr=%d,n_GVECl=%d,n_GVECr=%d,Total: %d numeric data fields\n",
	        EyeD->n_Time,EyeD->n_PORl,EyeD->n_PORr,EyeD->n_DIAl,EyeD->n_DIAr,EyeD->n_mDIAl,EyeD->n_mDIAr,
			EyeD->n_RAWl,EyeD->n_RAWr,EyeD->n_CRl,EyeD->n_CRr,EyeD->n_EPOSl,EyeD->n_EPOSr,EyeD->n_GVECl,EyeD->n_GVECr,EyeD->n_total_datafields);
	Rprintf("p_Time=%d,p_PORl=%d,p_PORr=%d,p_DIAl=%d,p_DIAr=%d,p_mDIAl=%d,p_mDIAr=%d,p_RAWl=%d,p_RAWr=%d,p_CRl=%d,p_CRr=%d,p_EPOSl=%d,p_EPOSr=%d,p_GVECl=%d,p_GVECr=%d\n",
	        EyeD->poz_Time,EyeD->poz_PORl,EyeD->poz_PORr,EyeD->poz_DIAl,EyeD->poz_DIAr,EyeD->poz_mDIAl,EyeD->poz_mDIAr,
			EyeD->poz_RAWl,EyeD->poz_RAWr,EyeD->poz_CRl,EyeD->poz_CRr,EyeD->poz_EPOSl,EyeD->poz_EPOSr,EyeD->poz_GVECl,EyeD->poz_GVECr);			
	}		
    if (rc != 4) Rprintf("ReadIViewStruct ERROR (rc=%d)\n",rc);	
    return (rc);
 }

//
// чтение файла данных 
//
SEXP readsmiC(SEXP fn, SEXP ReadRAW, SEXP ReadEPOS, SEXP ReadTrial, SEXP Debug)
{
	struct _stSMIeye EyeD; 
	int i,idx,nf,rc,reslen;
	char fname[400];
	char *n_Subj = "Subj";    char *n_N    = "N";        char *n_SampleRate = "SampleRate";	
	char *n_Area = "Area";    char *n_MsgTxt = "MsgTxt"; char *n_MsgTimes = "MsgTimes";
	char *n_HeadDist = "HeadDistance";
	char *n_Time = "Time";
	char *n_Trial = "Trial";
	char *n_lPorX = "L_PorX"; char *n_lPorY = "L_PorY";	char *n_rPorX = "R_PorX"; char *n_rPorY = "R_PorY";	
	char *n_lDX = "L_DX";     char *n_lDY = "L_DY";	char *n_rDX = "R_DX"; char *n_rDY = "R_DY";		
	char *n_lMP = "L_DM";      char *n_rMP = "R_DM";
	char *n_lRAWX = "L_RAWX"; char *n_lRAWY = "L_RAWY";	
	char *n_rRAWX = "R_RAWX"; char *n_rRAWY = "R_RAWY";	
	char *n_lCRX1 = "L_CRX1";  char *n_lCRY1 = "L_CRY1";	char *n_lCRX2 = "L_CRX2";  char *n_lCRY2 = "L_CRY2";
	char *n_rCRX1 = "R_CRX1";  char *n_rCRY1 = "R_CRY1";	char *n_rCRX2 = "R_CRX2";  char *n_rCRY2 = "R_CRY2";	
	char *n_Fn  = "Fn";
	char *n_lEPOS1 = "L_EPOSX"; char *n_lEPOS2 = "L_EPOSY"; char *n_lEPOS3 = "L_EPOSZ";
	char *n_rEPOS1 = "R_EPOSX"; char *n_rEPOS2 = "R_EPOSY"; char *n_rEPOS3 = "R_EPOSZ";	
	char *n_lGVEC1 = "L_GVECX"; char *n_lGVEC2 = "L_GVECY"; char *n_lGVEC3 = "L_GVECZ";	
	char *n_rGVEC1 = "R_GVECX"; char *n_rGVEC2 = "R_GVECY"; char *n_rGVEC3 = "R_GVECZ";
	char *names[MAX_N_FIELDS];
    //===
    SEXP Res, Sbj, Nrec, SampleRate, Area, MsgTxt, MsgTimes, HeadDist, list_names, ResM[MAX_N_FIELDS];
	//=======================================================================================================
    //аргументы - режимы чтени€ файла
	EyeD.m_ReadRAW  = INTEGER_VALUE(ReadRAW);
	EyeD.m_ReadEPOS = INTEGER_VALUE(ReadEPOS);
	EyeD.m_Debug    = INTEGER_VALUE(Debug);
	EyeD.m_ReadTrial = INTEGER_VALUE(ReadTrial);
	//аргумент - файл данных
	PROTECT(fn = AS_CHARACTER(fn)); 
    strcpy(fname,CHAR(STRING_ELT(fn,0)));
    UNPROTECT(1);
	if (EyeD.m_Debug > 1) {
	  Rprintf("readsmiC: fn=%s, ReadRAW=%d, ReadEPOS=%d\n",fname,EyeD.m_ReadRAW,EyeD.m_ReadEPOS);	
	}
	// открыть файл данных	
	EyeD.m_InFile=fopen(fname,"r");
	if (EyeD.m_InFile == NULL) {
		Rprintf("readsmiC ERROR: Cann't open file %s\n",fname);
	    return(R_NilValue);
	} 
	// получить структуру данных ======================================================
	if (EyeD.m_Debug > 1) {
	   Rprintf("readsmiC: call ReadIVewStruct\n");		
	}	
    rc=ReadIViewStruct(&EyeD);
	if (EyeD.m_Debug > 1) {
  	   Rprintf("readsmiC: rc=%d\n", rc);		
	}   
	if (rc != 4) return (R_NilValue);
	
	//== ѕереход на начало файла данных ===============================================
    fseek(EyeD.m_InFile,0L,SEEK_SET);	
    EyeD.m_NActualRecords=0;
    EyeD.m_CurrentFrame=0;
	EyeD.m_TimeStart=0;
	if (EyeD.m_Debug>1) {	
	   Rprintf("readsmiC: set names\n", rc);		
	}   
	idx=0;
    names[idx] = n_Subj; names[idx+1] = n_N; names[idx+2] = n_SampleRate;
	names[idx+3] = n_Area; names[idx+4] = n_HeadDist; 
	names[idx+5] = n_MsgTxt; names[idx+6] = n_MsgTimes; 	
	names[idx+7] = n_Time;  idx = idx+8;
	
	if (EyeD.n_Trial == 1) {
	    names[idx] = n_Trial; idx = idx+1;
	}	
    if (EyeD.n_PORl == 2) {
	    names[idx] = n_lPorX;  names[idx+1] = n_lPorY; idx = idx+2;
	}
    if (EyeD.n_PORr == 2) {
	    names[idx] = n_rPorX;  names[idx+1] = n_rPorY; idx = idx+2;
	}	
    if (EyeD.n_DIAl == 2) {
	    names[idx] = n_lDX; names[idx+1] = n_lDY; idx = idx+2;
	}	
    if (EyeD.n_DIAr == 2) {
	    names[idx] = n_rDX; names[idx+1] = n_rDY; idx = idx+2;
	}		
    if (EyeD.n_mDIAl == 1) {
	    names[idx] = n_lMP; idx = idx+1;
	}	
    if (EyeD.n_mDIAr == 1) {
	    names[idx] = n_rMP; idx = idx+1;
	}
	if (EyeD.n_RAWl == 2) {
	    names[idx] = n_lRAWX; names[idx+1] = n_lRAWY;  idx = idx+2;
	}
	if (EyeD.n_RAWr == 2) {
	    names[idx] = n_rRAWX; names[idx+1] = n_rRAWY;  idx = idx+2;
	}
	if (EyeD.n_CRl == 2) {
	    names[idx] = n_lCRX1; names[idx+1] = n_lCRY1;  idx = idx+2;
	}
	if (EyeD.n_CRr == 2) {
	    names[idx] = n_rCRX1; names[idx+1] = n_rCRY1;  idx = idx+2;
	}
	if (EyeD.n_CRl == 4) {
	    names[idx] = n_lCRX1; names[idx+1] = n_lCRY1;  names[idx+2] = n_lCRX2; names[idx+3] = n_lCRY2; idx = idx+4;
	}
	if (EyeD.n_CRr == 4) {
	    names[idx] = n_rCRX1; names[idx+1] = n_rCRY1;  names[idx+2] = n_rCRX2; names[idx+3] = n_rCRY2; idx = idx+4;
	}
	if (EyeD.n_EPOSl == 3) {
	    names[idx]   = n_lEPOS1; names[idx+1] = n_lEPOS2; names[idx+2] = n_lEPOS3; idx = idx+3;
    }
	if (EyeD.n_EPOSr == 3) {
	    names[idx]   = n_rEPOS1; names[idx+1] = n_rEPOS2; names[idx+2] = n_rEPOS3; idx = idx+3;
    }	
	if (EyeD.n_GVECl == 3) {
	    names[idx]   = n_lGVEC1; names[idx+1] = n_lGVEC2; names[idx+2] = n_lGVEC3; idx = idx+3;
    }
	if (EyeD.n_GVECr == 3) {
	    names[idx]   = n_rGVEC1; names[idx+1] = n_rGVEC2; names[idx+2] = n_rGVEC3; idx = idx+3;
    }	
	names[idx] = n_Fn; idx=idx+1;
	if (EyeD.m_Debug>1) {
	   for (i=0; i<idx;i++) {
	      Rprintf("readsmiC: names[%d]=%s\n", i, names[i]);
       }	
	} 
	if (EyeD.m_Debug>2) {
	    return (R_NilValue);
	}	
	///===================================================================================
	

    reslen = EyeD.n_total_datafields + 7; // + Subj, N, SampleRate, Area, HeadDist, MsgTxt, MsgTimes, ??Time
	if (EyeD.m_Debug>1) {
	     Rprintf("readsmiC: EyeD.n_total_datafields=%d\n", EyeD.n_total_datafields);
		 Rprintf("readsmiC: reslen=%d\n", reslen);
	}		
	PROTECT(Res = allocVector(VECSXP,reslen));	
	PROTECT(list_names = allocVector(STRSXP,reslen));
    PROTECT(Sbj = allocVector(STRSXP,1));
	PROTECT(Nrec = allocVector(REALSXP,1));
    PROTECT(SampleRate = allocVector(REALSXP,1));
    PROTECT(Area = allocVector(REALSXP,4));
	PROTECT(HeadDist = allocVector(REALSXP,1));	
    PROTECT(MsgTxt = allocVector(STRSXP,EyeD.m_NMsgRecords));
	PROTECT(MsgTimes = allocVector(REALSXP,EyeD.m_NMsgRecords));
	//===== main data matrix =======================================
	for(i=0; i< EyeD.n_total_datafields; i++) PROTECT(ResM[i] = allocVector(REALSXP,EyeD.m_NRecords));
	//data fill ====================================================
	SET_STRING_ELT(Sbj,0,mkChar(EyeD.m_SubjectName));
	REAL(Nrec)[0] = EyeD.m_NRecords;
	REAL(SampleRate)[0] = EyeD.m_SampleRate;
	REAL(Area)[0] = EyeD.m_AreaW;
	REAL(Area)[1] = EyeD.m_AreaH;
	REAL(Area)[2] = EyeD.m_AreaW_mm;	
	REAL(Area)[3] = EyeD.m_AreaH_mm;	
	REAL(HeadDist)[0] = EyeD.m_HeadDist_mm;
	while(1) {
	    rc = ScanSMIline(&EyeD);
		if (rc > 2)  break;
		if (rc == 0) {
	        //Rprintf("proceed: %d\n",EyeD.m_NActualRecords);
  	        nf = EyeD.n_total_datafields-1;
		    idx = EyeD.m_NActualRecords-1;
	        for (i=0; i<nf; i++) {
	 	        REAL(ResM[i])[idx] = EyeD.m_databuf[i];
		    }
		    REAL(ResM[nf])[idx] = EyeD.m_CurrentFrame;
	    }
		if (rc == 1) {
			i = EyeD.m_CurrentFrame -1;
			//Rprintf("i=%d msg=%s time=%f\n",i,EyeD.m_CrntMsgTxt,EyeD.m_CrntMsgTime);
			REAL(MsgTimes)[i] = EyeD.m_CrntMsgTime;
			SET_STRING_ELT(MsgTxt,i,mkChar(EyeD.m_CrntMsgTxt));
		}
	}	
	//=======================================================================
	//== Make output LIST data ==============================================	
	SET_VECTOR_ELT(Res,0,Sbj);
	SET_VECTOR_ELT(Res,1,Nrec);		 
	SET_VECTOR_ELT(Res,2,SampleRate);
	SET_VECTOR_ELT(Res,3,Area);
	SET_VECTOR_ELT(Res,4,HeadDist);
	SET_VECTOR_ELT(Res,5,MsgTxt);
	SET_VECTOR_ELT(Res,6,MsgTimes);


	for (i = 0; i < EyeD.n_total_datafields; i++) SET_VECTOR_ELT(Res,i+7,ResM[i]);
	//== Set names for output LIST elements  ===========================		 
    for(i = 0; i < reslen; i++)  SET_STRING_ELT(list_names,i,mkChar(names[i])); 	 
    setAttrib(Res, R_NamesSymbol, list_names); 			 
    UNPROTECT(9+EyeD.n_total_datafields);		 
	return(Res);
}


