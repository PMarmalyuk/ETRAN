/*****************************************************************************

File Name:       FIXFUNC.H

Compiler:        Microsoft C, Version 6.0

Company:         LC Technologies, Inc.
                 9455 Silver King Court
                 Fairfax, VA 22031
                 (703) 385-7133

Date Created:    04/12/95

*****************************************************************************/
#ifndef FIXFUNC_H_INCLUDED
#define FIXFUNC_H_INCLUDED
/****************************************************************************/
/* EYE MOTION STATES:                                                       */

#define MOVING                  0      /* return values for the             */
#define FIXATING                1      /*   detect_fixation() function      */
#define FIXATION_COMPLETED      2      /*                                   */

/****************************************************************************/
#ifdef __cplusplus
extern "C" {            /* Assume C declarations for C++ */
#endif  /* __cplusplus */

/****************************************************************************/
/*  FUNCTION PROTOTYPE:                                                     */

void InitFixation(int iMinimumFixSamples);

int  DetectFixation(
                    int    bGazepointFound,
                    float  fXGaze,
                    float  fYGaze,
                    float  fGazeDeviationThreshold,
                    int    iMinimumFixSamples,

                    int   *pbGazepointFoundDelayed,
                    float *pfXGazeDelayed,
                    float *pfYGazeDelayed,
                    float *pfGazeDeviationDelayed,

                    float *pfXFixDelayed,
                    float *pfYFixDelayed,
                    int   *piSaccadeDurationDelayed,
                    int   *piFixDurationDelayed);

/****************************************************************************/
#ifdef __cplusplus
}          /* Assume C declarations for C++ */
#endif  /* __cplusplus */
/****************************************************************************/
#endif // FIXFUNC_H_INCLUDED
/****************************************************************************/

