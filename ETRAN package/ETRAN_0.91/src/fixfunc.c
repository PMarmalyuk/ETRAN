/*****************************************************************************

File Name:       FIXFUNC.C
Program Name:    Eye Fixation Analysis Functions

Company:         LC Technologies, Inc.
                 9455 Silver King Court
                 Fairfax, VA 22031
                 (703) 385-7133

Date Created:    10/20/89
                 04/12/95 modified: turned into collection of functions

*****************************************************************************/

/****************************************************************************/
/* FUNCTION, VARIABLE AND CONSTANT DEFINITONS:                              */

#include <stdio.h>
#include <conio.h>
#include <math.h>
#include <string.h>
#include <process.h>
#include "lctypdef.h"
//#include <lctsupt.h>
#include "fixfunc.h"
//#include <crtdbg.h>              /* debug tools                             */

#define RING_SIZE             1251     /* length of the delay line in       */
                                       /*   DetectFixation() --             */
                                       /*   should be greater than          */
                                       /*   iMinimumFixSamples              */
#define NEW_FIX   0
#define PRES_FIX  1
#define PREV_FIX  2  

/****************************************************************************/
/* FUNCTION PROTOTYPES:                                                     */

void  ResetFixation(int iNewPresPrev);
void  StartFixAtGazepoint(int iNewPresPrev, float fXGaze, float fYGaze);
void  UpdateFixation(int iNewPresPrev,
                      float fXGaze, float fYGaze, int iMininumFixSamples);
float CalcGazeDeviationFromFix(int iNewPresPrev, float fXGaze, float fYGaze);
void  CheckIfFixating(int iMinimumFixSamples);
void  MoveNewFixToPresFix(int iMinimumFixSamples);
void  DeclareCompletedFixation(int iMinimumFixSamples, int iSamplesAgo);
void  RestoreOutPoints(void);

/****************************************************************************/
/* STRUCTURES DEFINITIONS:                                                  */

struct _stFix                   /* FIXATION DATA                            */
{
   int   iStartCount;           /* call count that the fixation starts      */
   int   iEndCount;             /* call count that the fixation ends        */
   int   iNSamples;             /* number of samples in the present fix     */
   float fXSum;                 /* summations for calculation of average    */
   float fYSum;                 /*   fixation position                      */
   float fX;                    /* average coordinate of the eye fixation   */
   float fY;                    /*   point (user selected units)            */
};

struct _stRingBuf               /* RING BUFFER STORING PAST VALUES:         */
{
    float fXGaze;               /* gazepoint coordinate                     */
    float fYGaze;               /*                                          */
    BOOL  bGazeFound;           /* gazepoint found flag                     */
    int   iEyeMotionState;      /* state of the eye motion:                 */
                                /*   MOVING                                 */
                                /*   FIXATING                               */
                                /*   FIXATION_COMPLETED                     */
    float fXFix;                /* current fixation coordinate              */
    float fYFix;                /*                                          */
    float fGazeDeviation;       /* gazepoint deviation from the fixation    */
    int   iSacDuration;         /* saccade duration                         */
    int   iFixDuration;         /* fixation duration                        */
};

/****************************************************************************/
/* GLOBAL FIXFUNC VARIABLES:                                                */

int   iCallCount;               /* number of times this function has been   */
                                /*   called since it was initialized        */
                                /*   (30ths or 60ths of a second,           */
                                /*   depending on eyetracking sample rate)  */
int   iNPresOut;                /* number of successive gazepoint samples   */
                                /*   outside the present fixation circle    */

int   iNSamplesSinceLastGoodFixPoint;
                                /* number of samples since the last known   */
                                /*   good gazepoint in the present fixation */
                                /* Note: During continuous tracking of a    */
                                /*   normal fixation, this value is 1.      */
float fPresDr;                  /* difference between gazepoint and         */
                                /*   fixation (x, y, and radius)            */
float fNewDr;                   /* difference between gazepoint and         */
                                /*   fixation (x, y, and radius)            */
int   iMaxMissedSamples;        /* maximum number of successive gazepoint   */
                                /*   samples that may go untracked before   */
                                /*   a fixation is allowed to continue      */
int   iMaxOutSamples;           /* maximum number of successive gazepoint   */
                                /*   samples that may go outside the        */
                                /*   fixation circle                        */
struct _stFix stFix[3];         /* prior, present and new fixations         */
                                /*   indexed by: NEW_FIX   0                */
                                /*               PRES_FIX  1                */
                                /*               PREV_FIX  2                */
struct _stRingBuf stRingBuf[RING_SIZE];
int   iRingIndex;               /* ring index of the present gaze sample    */
int   iRingIndexDelay;          /* ring index of the gaze sample taken      */
                                /*   iMinimumFixSamples ago                 */

#if defined DIAG_FIXFUNC
   FILE  *hfFixFile;
#endif   /* DIAG_FIXFUNC */

/****************************************************************************/
void InitFixation(int iMinimumFixSamples)
                                       /* minimum number of gaze samples    */
                                       /*   that can be considered a        */
                                       /*   fixation                        */
                                       /*   Note: if the input value is     */
                                       /*   is less than 3, the function    */
                                       /*   sets it to 3                    */

/* This function clears any previous, present and new fixations, and it     */
/* initializes DetectFixation()'s internal ring buffers of prior            */
/* gazepoint data.  InitFixation() should be called prior to a sequence     */
/* of calls to DetectFixation().                                            */

{
/* Set the maximum allowable number of consecutive samples that may go      */
/* untracked within a fixation.                                             */
   iMaxMissedSamples = 3;

/* Set the maximum allowable number of consecutive samples that may go      */
/* outside the fixation circle.                                             */
   iMaxOutSamples = 1;

/* Initialize the internal ring buffer.                                     */
   for (iRingIndex = 0; iRingIndex < RING_SIZE; iRingIndex++)
   {
      stRingBuf[iRingIndex].fXGaze          =  0.0F;
      stRingBuf[iRingIndex].fYGaze          =  0.0F;
      stRingBuf[iRingIndex].bGazeFound      =  FALSE;
      stRingBuf[iRingIndex].iEyeMotionState =  MOVING;
      stRingBuf[iRingIndex].fXFix           =  0.0F;
      stRingBuf[iRingIndex].fYFix           =  0.0F;
      stRingBuf[iRingIndex].fGazeDeviation  = -0.1F;
      stRingBuf[iRingIndex].iSacDuration    =  0;
      stRingBuf[iRingIndex].iFixDuration    =  0;
   }
   iRingIndex = 0;
   iRingIndexDelay = RING_SIZE - iMinimumFixSamples;

/* Set the call count to zero, and initialize the previous fixation end     */
/* count so the first saccade duration is a legitimate count.               */
   iCallCount                = 0;
   stFix[PREV_FIX].iEndCount = 0;

/* Reset the present fixation data.                                         */
   ResetFixation(PRES_FIX);

/* Reset the new fixation data.                                             */
   ResetFixation(NEW_FIX);
}
/****************************************************************************/
int DetectFixation(
                                       /* INPUT PARAMETERS:                 */
       int   bGazepointFound,          /* flag indicating whether or not    */
                                       /*   the image processing algo       */
                                       /*   detected the eye and computed   */
                                       /*   a valid gazepoint (TRUE/FALSE)  */
       float fXGaze,                   /* present gazepoint                 */
       float fYGaze,                   /*   (user specified units)          */
       float fGazeDeviationThreshold,  /* distance that a gazepoint may     */
                                       /*   vary from the average fixation  */
                                       /*   point and still be considered   */
                                       /*   part of the fixation            */
                                       /*   (user specified units)          */
       int   iMinimumFixSamples,       /* minimum number of gaze samples    */
                                       /*   that can be considered a        */
                                       /*   fixation                        */
                                       /*   Note: if the input value is     */
                                       /*   is less than 3, the function    */
                                       /*   sets it to 3                    */

                                       /* OUTPUT PARAMETERS:                */
                                       /* Delayed Gazepoint data with       */
                                       /*   fixation annotations:           */
       int   *pbGazepointFoundDelayed,
                                       /* sample gazepoint-found flag,      */
                                       /*   iMinimumFixSamples ago          */
       float *pfXGazeDelayed,          /* sample gazepoint coordinates,     */
       float *pfYGazeDelayed,          /*   iMinimumFixSamples ago          */
       float *pfGazeDeviationDelayed,
                                       /* deviation of the gaze from the    */
                                       /*   present fixation,               */
                                       /*   iMinimumFixSamples ago          */

                                       /* Fixation data - delayed:          */
       float *pfXFixDelayed,           /* fixation point as estimated       */
       float *pfYFixDelayed,           /*   iMinimumFixSamples ago          */
       int   *piSaccadeDurationDelayed,
                                       /* duration of the saccade           */
                                       /*   preceeding the preset fixation  */
                                       /*   (samples)                       */
       int   *piFixDurationDelayed)    /* duration of the present fixation  */
                                       /*   (samples)                       */

/* RETURN VALUES - Eye Motion State:                                        */
/*                                                                          */
/*  MOVING                0   The eye was in motion iMinimumFixSamples ago. */
/*  FIXATING              1   The eye was fixating iMinimumFixSamples ago.  */
/*  FIXATION_COMPLETED    2   A completed fixation has just been detected.  */
/*                              With respect to the sample that reports     */
/*                              FIXATION_COMPLETED, the fixation started    */
/*                              (iMinimumFixSamples +                       */
/*                               *piSaccadeDurationDelayed)                 */
/*                              ago and ended iMinimumFixSamples ago.       */
/*                              Note, however, that because of the          */
/*                              (approximately 2-field) measurement latency */
/*                              in the eyetracking measurement, the start   */
/*                              and end times occurred (iMinimumFixSamples  */
/*                              + *piSaccadeDurationDelayed + 2) and        */
/*                              (iMinimumFixSamples + 2) ago with respect   */
/*                              to real time now.                           */
/*                                                                          */
/* Include FIXFUNC.H for function prototype and above constant definitions. */
/*                                                                          */
/* SUMMARY                                                                  */
/*                                                                          */
/*    This function converts a series of uniformly-sampled (raw) gaze       */
/* points into a series of variable-duration saccades and fixations.        */
/* Fixation analysis may be performed in real time or after the fact.  To   */
/* allow eye fixation analysis during real-time eyegaze data collection,    */
/* the function is designed to be called once per sample.  When the eye     */
/* is in motion, ie during saccades, the function returns 0 (MOVING).       */
/* When the eye is still, ie during fixations, the function returns 1       */
/* (FIXATING).  Upon the detected completion of a fixation, the function    */
/* returns 2 (FIXATION_COMPLETED) and produces:                             */
/*   a) the time duration of the saccade between the last and present       */
/*      eye fixation (eyegaze samples)                                      */
/*   b) the time duration of the present, just completed fixation           */
/*      (eyegaze samples)                                                   */
/*   c) the average x and y coordinates of the eye fixation                 */
/*      (in user defined units of fXGaze and fYGaze)                        */
/* Note: Although this function is intended to work in "real time", there   */
/* is a delay of iMinimumFixSamples in the filter which detects the         */
/* motion/fixation condition of the eye.                                    */
/*                                                                          */
/* PRINCIPLE OF OPERATION                                                   */
/*                                                                          */
/*    This function detects fixations by looking for sequences of gaze-     */
/* point measurements that remain relatively constant.  If a new gazepoint  */
/* lies within a circular region around the running average of an on-going  */
/* fixation, the fixation is extended to include the new gazepoint.         */
/* (The radius of the acceptance circle is user specified by setting the    */
/* value of the function argument fGazeDeviationThreshold.)                 */
/*    To accommodate noisy eyegaze measurements, a gazepoint that exceeds   */
/* the deviation threshold is included in an on-going fixation if the       */
/* subsequent gazepoint returns to a position within the threshold.         */
/*    If a gazepoint is not found, during a blink for example, a fixation   */
/* is extended if a) the next legitimate gazepoint measurement falls within */
/* the acceptance circle, and b) there are less than iMinimumFixSamples     */
/* of successive missed gazepoints.  Otherwise, the previous fixation       */
/* is considered to end at the last good gazepoint measurement.             */
/*                                                                          */
/* UNITS OF MEASURE                                                         */
/*                                                                          */
/*    The gaze position/direction may be expressed in any units (e.g.       */
/* millimeters, pixels, or radians), but the filter threshold must be       */
/* expressed in the same units.                                             */
/*                                                                          */
/* INITIALIZING THE FUNCTION                                                */
/*                                                                          */
/*    Prior to analyzing a sequence of gazepoint data, the InitFixation     */
/* function should be called to clear any previous, present and new         */
/* fixations and to initialize the ring buffers of prior gazepoint data.    */
/*                                                                          */
/* PROGRAM NOTES                                                            */
/*                                                                          */
/* For purposes of describing an ongoing sequence of fixations, fixations   */
/* in this program are referred to as "previous", "present", and "new".     */
/* The present fixation is the one that is going on right now, or, if a     */
/* new fixation has just started, the present fixation is the one that      */
/* just finished.  The previous fixation is the one immediatly preceeding   */
/* the present one, and a new fixation is the one immediately following     */
/* the present one.  Once the present fixation is declared to be completed, */
/* the present fixation becomes the previous one, the new fixation becomes  */
/* the present one, and there is not yet a new fixation.                    */

/*--------------------------------------------------------------------------*/
{
   #if defined DIAG_FIXFUNC 
     fPresDr = -0.1F;
     fNewDr  = -0.1F;
   #endif   /* DIAG_FIXFUNC */

/* Make sure the minimum fix time is at least 3 samples.                    */
   if (iMinimumFixSamples < 3) iMinimumFixSamples = 3;

/* Make sure the ring size is large enough to handle the delay.             */
   if (iMinimumFixSamples >= RING_SIZE)
   {
      _tprintf(_TEXT("iMinimumFixSamples %i >= RING_SIZE %i\n"),
                      iMinimumFixSamples,      RING_SIZE);
      _tprintf(_TEXT("Press any key to terminate..."));
      _getch();
      exit(99);
   }

/* Increment the call count, the ring index, and the delayed ring index.    */
   iCallCount++;
   iRingIndex++;
   if (iRingIndex >= RING_SIZE) iRingIndex = 0;
   iRingIndexDelay = iRingIndex - iMinimumFixSamples;
   if (iRingIndexDelay < 0) iRingIndexDelay += RING_SIZE;

//   _ASSERTE((iRingIndex >= 0) && (iRingIndex < RING_SIZE));
//   _ASSERTE((iRingIndexDelay >= 0) && (iRingIndexDelay < RING_SIZE));

/* Update the ring buffer of past gazepoints.                               */
   stRingBuf[iRingIndex].fXGaze     = fXGaze;
   stRingBuf[iRingIndex].fYGaze     = fYGaze;
   stRingBuf[iRingIndex].bGazeFound = bGazepointFound;

/* Initially assume the eye is moving.                                      */
/* Note: These values are updated during the processing of this and         */
/* subsequent gazepoints.                                                   */
   stRingBuf[iRingIndex].iEyeMotionState = MOVING;
   stRingBuf[iRingIndex].fXFix           = -0.0F;
   stRingBuf[iRingIndex].fYFix           = -0.0F;
   stRingBuf[iRingIndex].fGazeDeviation  = -0.1F;
   stRingBuf[iRingIndex].iSacDuration    =  0;
   stRingBuf[iRingIndex].iFixDuration    =  0;

/* - - - - - - - - - Samples Since Last Good Gazepoint - - - - - - - - - - -*/

/* Compute the number of samples since the last known good gazepoint        */
/* in the present fixation:                                                 */
/* Note: During the tracking of a normal fixation, this number is 1.        */
/* If there is a present fixation,                                          */
   if (stFix[PRES_FIX].iEndCount > 0)
   {
/*    The number is equal to the difference between the present call count  */
/*    and the end-count of the present fixation.                            */
      iNSamplesSinceLastGoodFixPoint =
         iCallCount - stFix[PRES_FIX].iEndCount;
   }
/* Otherwise, if there is no present fixation hypothesis,                   */
   else
   {
/*    Assume the last sample was good, so difference is 1.                  */
      iNSamplesSinceLastGoodFixPoint = 1;
   }

/*- - - - - - - - - - - - - Process Tracked Eye  - - - - - - - - - - - - - -*/

/* A1 If the eye's gazepoint was successfully measured this sample,         */
   if (bGazepointFound == TRUE)
   {
/* B1 If a present fixation exists or is presently hypothesized,            */
      if (stFix[PRES_FIX].iNSamples > 0)
      {
/*       Compute the deviation of the gazepoint from the present fixation.  */
         fPresDr = CalcGazeDeviationFromFix(PRES_FIX, fXGaze, fYGaze);

/*   C1  If the gazepoint is within the present fixation circle,            */
         if (fPresDr <= fGazeDeviationThreshold)
         {
/*          Restore any previous gazepoints that were temporarily left      */
/*          out of the present fixation.                                    */
            RestoreOutPoints();

/*          Update the present fixation hypothesis, and check if there      */
/*          are enough samples to declare that the eye is fixating.         */
            UpdateFixation(PRES_FIX, fXGaze, fYGaze, iMinimumFixSamples);
         }

/*   C2  Otherwise, if the point is outside the present fixation circle,    */
         else   /* if (fPresDr > fGazeDeviationThreshold) */
         {
/*          Increment the number of successive gazepoint samples outside    */
/*          the present fixation circle.                                    */
            iNPresOut++;

/*      D1  If the number of successive gazepoints outside the present      */
/*          fixation circle has not exceeded its maximum,                   */
            if (iNPresOut <= iMaxOutSamples)
            {
/*             Incorporate this gazepoint into the NEW fixation hypothesis: */
/*         E1  If a new fixation hypothesis has already been started,       */
               if (stFix[NEW_FIX].iNSamples > 0)
               {
/*                Compute the gazepoint's deviation from the new fixation.  */
                  fNewDr = CalcGazeDeviationFromFix(NEW_FIX, fXGaze, fYGaze);

/*            F1  If the new gazepoint falls within the new fix,            */
                  if (fNewDr <= fGazeDeviationThreshold)
                  {
/*                   Update the new fixation hypothesis.                    */
                     UpdateFixation(NEW_FIX, fXGaze, fYGaze, iMinimumFixSamples);
                  }

/*            F2  Otherwise, if the point is outside the new fix,           */
                  else   /* if (fNewDr <= fGazeDeviationThreshold) */
                  {
/*                   Reset the new fixation at this new gazepoint.          */
                     StartFixAtGazepoint(NEW_FIX, fXGaze, fYGaze);
                  }
               }

/*         E2  Otherwise, If there is not a new fix,                        */
               else   /* if (n_new_fix_counts == 0) */
               {
/*                Start a new fixation hypothesis at this gazepoint.        */
                  StartFixAtGazepoint(NEW_FIX, fXGaze, fYGaze);
               }
            }

/*      D2  Otherwise, if too many successive gazepoint samples have        */
/*          gone outside the fixation circle,                               */
            else // iNPresOut > iMaxOutSamples
            {
/*             The present fixation must either be declared complete or     */
/*             rejected:                                                    */
/*         G1  If the present fixation is long enough to count as a real    */
/*             fixation,                                                    */
               if (stFix[PRES_FIX].iNSamples >= iMinimumFixSamples)
               {
/*                Declare the present fixation to be completed at the       */
/*                last good sample, move the present  fixation to the       */
/*                prior, and move the new fixation to the present.          */
                  DeclareCompletedFixation(iMinimumFixSamples, iNSamplesSinceLastGoodFixPoint);
               }
/*         G2  Otherwise, if the present fixation not is long enough to     */
/*             count as a real fixation,                                    */
               else
               {
/*                Reject the present fixation by replacing it with the      */
/*                new fixation (which may or may not exist at this time).   */
                  MoveNewFixToPresFix(iMinimumFixSamples);
               }

/*         H1  If there is a present-fixation hypothesis                    */
               if (stFix[PRES_FIX].iNSamples > 0)
               {
/*                Compute the deviation of the gazepoint from the now       */
/*                present fixation.                                         */
                  fPresDr = CalcGazeDeviationFromFix(PRES_FIX, fXGaze, fYGaze);

/*                If the gazepoint is within the now present fixation       */
/*                circle,                                                   */
                  if (fPresDr <= fGazeDeviationThreshold)
                  {
/*                   Update the present fixation data, and check if there   */
/*                   are enough samples to declare that the eye is fixating.*/
                     UpdateFixation(PRES_FIX, fXGaze, fYGaze, iMinimumFixSamples);
                  }

/*                Otherwise, if the gazepoint falls outside the present     */
/*                fixation,                                                 */
                  else   /* if (fPresDr > fGazeDeviationThreshold) */
                  {
/*                   Start a new fixation hypothesis at this gazepoint.     */
                     StartFixAtGazepoint(NEW_FIX, fXGaze, fYGaze);
                  }
               }

/*         H2  Otherwise, if there is no present-fix hypothesis,            */
               else  // if (stFix[PRES_FIX].iNSamples == 0)
               {
/*                Start a present fixation at this gazepoint.               */
                  StartFixAtGazepoint(PRES_FIX, fXGaze, fYGaze);
               }
            }
         }
      }

/* B2 Otherwise, if there is not a present fixation going,                  */
      else  /* if (stFix[PRES_FIX].iNSamples == 0) */
      {
/*       Start the present fixation at the gazepoint and reset the          */
/*       new fixation.                                                      */
         StartFixAtGazepoint(PRES_FIX, fXGaze, fYGaze);
      }
   }

/*- - - - - - - - - - - - - Process Untracked Eye  - - - - - - - - - - - - -*/

/* A2 Otherwise, if the eye's gazepoint was not successfully measured       */
/* this sample,                                                             */
   else   /*  if (bGazepointFound == FALSE)  */
   {
/* I1 If there is still time to update the present fixation, i.e. the last  */
/*    good sample in the present fixation occurred within the permissible   */
/*    time gap,                                                             */

      if (iNSamplesSinceLastGoodFixPoint <= iMaxMissedSamples)
      {
/*       No action is to be taken (no code).                                */
      }

/* I2 Otherwise, if too much time has passed since the last good gazepoint  */
/*    in the present fixation,                                              */
      else  // if (iNSamplesSinceLastGoodFixPoint >= iMaxMissedSamples)
      {
/*       The present fixation must be declared complete or rejected:        */
/*   J1  If the present fixation is long enough to count as a real          */
/*       fixation,                                                          */
         if (stFix[PRES_FIX].iNSamples >= iMinimumFixSamples)
         {
/*          Declare the present fixation to be completed at the last good   */
/*          sample, move the present  fixation to the prior, and move the   */
/*          new fixation to the present.                                    */
            DeclareCompletedFixation(iMinimumFixSamples, iNSamplesSinceLastGoodFixPoint);
         }

/*   J2  Otherwise, if the present fixation not is long enough to count     */
/*       as a real fixation,                                                */
         else
         {
/*          Reject the present fixation by replacing it with the new        */
/*          fixation (which may or may not exist at this time).             */
            MoveNewFixToPresFix(iMinimumFixSamples);
         }
      }
   }

/*---------------------------- Pass Data Back ------------------------------*/

//   _ASSERTE((iRingIndexDelay >= 0) && (iRingIndexDelay < RING_SIZE));

/* Pass the delayed gazepoint data, with the relevant saccade/fixation      */
/* data, back to the calling function.                                      */
   *pfXGazeDelayed          = stRingBuf[iRingIndexDelay].fXGaze;
   *pfYGazeDelayed          = stRingBuf[iRingIndexDelay].fYGaze;
   *pbGazepointFoundDelayed = stRingBuf[iRingIndexDelay].bGazeFound;
   *pfXFixDelayed           = stRingBuf[iRingIndexDelay].fXFix;
   *pfYFixDelayed           = stRingBuf[iRingIndexDelay].fYFix;
   *pfGazeDeviationDelayed  = stRingBuf[iRingIndexDelay].fGazeDeviation;
   *piSaccadeDurationDelayed= stRingBuf[iRingIndexDelay].iSacDuration;
   *piFixDurationDelayed    = stRingBuf[iRingIndexDelay].iFixDuration;

/* Record diagnostic data.                                                  */
   #if defined DIAG_FIXFUNC 
     RecordDiagnosticData(bGazepointFound, fXGaze, fYGaze,
        fGazeDeviationThreshold, iMinimumFixSamples,
        *pfXFixDelayed, *pfYFixDelayed,
        *piSaccadeDurationDelayed, *piFixDurationDelayed);
   #endif   /* DIAG_FIXFUNC */

/* Return the eye motion/fixation state for the delayed point.              */
   return stRingBuf[iRingIndexDelay].iEyeMotionState;
}
/****************************************************************************/
void ResetFixation(int iNewPresPrev)

/* This function resets the argument NewPresPrev fixation, i.e. declares    */
/* it nonexistent.                                                          */

{
   stFix[iNewPresPrev].iStartCount = 0;
   stFix[iNewPresPrev].iEndCount   = 0;
   stFix[iNewPresPrev].iNSamples   = 0;
   stFix[iNewPresPrev].fXSum       = 0.0F;
   stFix[iNewPresPrev].fYSum       = 0.0F;
   stFix[iNewPresPrev].fX          = 0.0F;
   stFix[iNewPresPrev].fY          = 0.0F;

   if (iNewPresPrev == PRES_FIX)
   {
      iNPresOut = 0;
   }
}
/****************************************************************************/
void StartFixAtGazepoint(int iNewPresPrev, float fXGaze, float fYGaze)

/* This function starts the argument NewPresPrev fixation at the argument   */
/* gazepoint and makes sure there is no new fixation hypothesis.            */

{

/* Start the present fixation at the argument gazepoint.                    */
   stFix[iNewPresPrev].iNSamples   = 1;
   stFix[iNewPresPrev].fXSum       = fXGaze;
   stFix[iNewPresPrev].fYSum       = fYGaze;
   stFix[iNewPresPrev].fX          = fXGaze;
   stFix[iNewPresPrev].fY          = fYGaze;
   stFix[iNewPresPrev].iStartCount = iCallCount;
   stFix[iNewPresPrev].iEndCount   = iCallCount;

/* If starting the present fixation,                                        */
   if (iNewPresPrev == PRES_FIX)
   {
      iNPresOut = 0;

/*    Make sure there is no new fixation.                                   */
      ResetFixation(NEW_FIX);
   }
}
/****************************************************************************/
void UpdateFixation(int iNewPresPrev, float fXGaze, float fYGaze, int iMinimumFixSamples)

/* This function updates the argument NewPresPrev fixation with the         */
/* argument gazepoint, checks if there are enough samples to declare that   */
/* the eye is now fixating, and makes sure there is no hypothesis for a     */
/* new fixation.                                                            */

{
/* Update the present fixation with the argument gazepoint.                 */
   stFix[iNewPresPrev].iNSamples++;
   stFix[iNewPresPrev].fXSum    += fXGaze;
   stFix[iNewPresPrev].fYSum    += fYGaze;
   stFix[iNewPresPrev].fX        = stFix[iNewPresPrev].fXSum / stFix[iNewPresPrev].iNSamples;
   stFix[iNewPresPrev].fY        = stFix[iNewPresPrev].fYSum / stFix[iNewPresPrev].iNSamples;
   stFix[iNewPresPrev].iEndCount = iCallCount;

/* If starting the present fixation,                                        */
   if (iNewPresPrev == PRES_FIX)
   {
      iNPresOut = 0;

/*    Check if there are enough samples in the present fixation hypothesis  */
/*    to declare that the eye is fixating.                                  */
      CheckIfFixating(iMinimumFixSamples);

/*    There is no hypothesis for a new fixation.                            */
      ResetFixation(NEW_FIX);
   }
}
/****************************************************************************/
float CalcGazeDeviationFromFix(int iNewPresPrev, float fXGaze, float fYGaze)

/* This function calculates the deviation of the gazepoint from the         */
/* argument NewPresPrev fixation location.                                  */

{
   float  fDx, fDy;                /* horizontal and vertical deviations    */
   float  fDr;                     /* total gazepoint deviation from the    */
                                   /*   current fixation point              */
   double dDrSq;                   /* square of the gazepoint deviation     */

   fDx = fXGaze - stFix[iNewPresPrev].fX;
   fDy = fYGaze - stFix[iNewPresPrev].fY;
   dDrSq = fDx * fDx + fDy * fDy;
//   _ASSERTE(dDrSq >= 0.0);
   if      (dDrSq <  0.0)
            dDrSq =  0.0;
   fDr = (float)sqrt(dDrSq);

//   _ASSERTE((iRingIndex >= 0) && (iRingIndex < RING_SIZE));

/* If starting the present fixation,                                        */
   if (iNewPresPrev == PRES_FIX)
   {
/*    Put the deviation in the ring buffer for future reference.            */
      stRingBuf[iRingIndex].fGazeDeviation = fPresDr;
   }

   return fDr;
}
/****************************************************************************/
void CheckIfFixating(int iMinimumFixSamples)

/* This function checks to see whether there are enough samples in the      */
/* PRESENT fixation to declare that the eye is fixating yet, and if there   */
/* is a true fixation going on, it updates the ring buffers to reflect      */
/* the fixation.                                                            */

{
   int  i, ii;                  /* dummy ring indices                       */

/* If there are enough samples for a fixation,                              */
   if (stFix[PRES_FIX].iNSamples >= iMinimumFixSamples)
   {
/*    Declare the eye to be fixating.  Go back through the last             */
/*    iMinimumFixSamples entries of the ring buffer making sure that all    */
/*    samples from the present fixation are marked as fixating, and set     */
/*    the entries with the newest estimate of the fixation location.        */
      for (i = 0; i < iMinimumFixSamples; i++)
      {
         ii = iRingIndex - i;
         if (ii < 0) ii += RING_SIZE;

//          _ASSERTE((ii >= 0) && (ii < RING_SIZE));

         stRingBuf[ii].iEyeMotionState = FIXATING;
         stRingBuf[ii].fXFix = stFix[PRES_FIX].fX;
         stRingBuf[ii].fYFix = stFix[PRES_FIX].fY;

         stRingBuf[ii].iSacDuration = stFix[PRES_FIX].iStartCount - stFix[PREV_FIX].iEndCount - 1;
         stRingBuf[ii].iFixDuration = stFix[PRES_FIX].iEndCount - i - stFix[PRES_FIX].iStartCount + 1;
      }
   }
}
/****************************************************************************/
void MoveNewFixToPresFix(int iMinimumFixSamples)

/* This function copies the new fixation data into the present fixation,    */
/* and resets the new fixation.                                             */

{
/* Reset the number of accumulated points outside the priesent fixation.    */
   iNPresOut = 0;

/* Move the new fixation to the present fixation.                           */
   stFix[PRES_FIX] = stFix[NEW_FIX];

/* Reset the number of gazepoints outside the fixation.                     */
   iNPresOut = 0;

/* Reset the new fixation.                                                  */
   ResetFixation(NEW_FIX);

/* Check if there are enough samples in the new (now present) fixation to   */
/* declare that the eye is fixating.                                        */
   CheckIfFixating(iMinimumFixSamples);
}
/****************************************************************************/
void DeclareCompletedFixation(int iMinimumFixSamples, int iSamplesAgo)

/* This function:                                                           */
/* a) declares the present fixation to be completed as of iSamplesAgo,      */
/* b) moves the present fixation to the prior fixation, and                 */
/* c) moves the new fixation, if any, to the present fixation.              */

{
   int iRingIndexFixCompleted;   /* ring index corresponding to the         */
                                 /*   fixation-complete time                */

/* Compute the ring index corresponding to the fixation-complete time.      */
   iRingIndexFixCompleted = iRingIndex - iSamplesAgo;
   if (iRingIndexFixCompleted < 0) iRingIndexFixCompleted += RING_SIZE;

/* Declare the present fixation to be completed.                            */
   stRingBuf[iRingIndexFixCompleted].iEyeMotionState = FIXATION_COMPLETED;

/* Move the present fixation to the previous fixation.                      */
   stFix[PREV_FIX] = stFix[PRES_FIX];

/* Move the new fixation data, if any, to the present fixation, reset       */
/* the new fixation, and check if there are enough samples in the new       */
/* (now present) fixation to declare that the eye is fixating.              */
   MoveNewFixToPresFix(iMinimumFixSamples);
}
/****************************************************************************/
void RestoreOutPoints(void)

/* This function restores any previous gazepoints that were left out of     */
/* the fixation and are now known to be part of the present fixation.       */

{
   int  i, ii;                  /* dummy ring indices                       */

/* If there were some previous points that temporarily went out of the      */
/* fixation circle,                                                         */
   if (iNSamplesSinceLastGoodFixPoint > 1)
   {
/*    Undo the hypothesis that they were outside the fixation and declare   */
/*    them now to be part of the fix.                                       */
      for (i = 1; i < iNSamplesSinceLastGoodFixPoint; i++)
      {
         ii = iRingIndex - i;
         if (ii < 0) ii += RING_SIZE;

//          _ASSERTE((ii >= 0) && (ii < RING_SIZE));

         if (stRingBuf[ii].bGazeFound == TRUE)
         {
            stFix[PRES_FIX].iNSamples++;
            stFix[PRES_FIX].fXSum        += stRingBuf[ii].fXGaze;
            stFix[PRES_FIX].fYSum        += stRingBuf[ii].fYGaze;
            stRingBuf[ii].iEyeMotionState = FIXATING;
         }
      }

/*    Reset the number of "out" points to zero.                             */
      iNPresOut = 0;
   }
}
/****************************************************************************/
