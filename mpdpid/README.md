2018-02-09-README

MpdPid How To
=============

* * *

The following instruction provides the description how to use MpdPid class in analysis.

Table of content:
-----------------

1.  [MpdPid class](#MpdPid)
   *   [1.1 Constructor with arguments](#MpdPid_1)
   *   [1.2 How to set protons/antiprotons ratio](#MpdPid_2)
   *   [1.3 Probability vector](#MpdPid_3)
       *   [1.3.1 How to fill probability vector](#MpdPid_3_1)
       *   [1.3.2 How to get probabilities](#MpdPid_3_2)
       *   [1.3.3 Getting the most probable pdg value](#MpdPid_3_3)
       *   [1.3.4 Example](#MpdPid_3_4)
   *   [1.4 Track selection criteria](#MpdPid_4)
2.  [MpdPidQA class](#MpdPidQA)
   *   [2.1 Constructor with arguments](#MpdPidQA_1)
   *   [2.2 Usage](#MpdPidQA_2)
       *   [2.2.1 How to fill QA histograms](#MpdPidQA_2_1)
       *   [2.2.2 How to save results](#MpdPidQA_2_2)
       *   [2.2.3 Example](#MpdPidQA_2_3)
   *   [2.3 Interpretation of results](#MpdPidQA_3)

<a name="MpdPid"></a>
1\. MpdPid class
----------------
<a name="MpdPid_1"></a>
### 1.1 Constructor with arguments

Constructor of MpdPid class:

```
MpdPid(Double_t sigM, Double_t sigE, Double_t E, Double_t C, TString generator, TString tracking, TString nSigPart);
```

1.  `sigM` ‒ non-zero distance from the average mass-squared value (in terms of standard deviations);
2.  `sigE` ‒ non-zero distance from the average dE/dx value (in terms of standard deviations);
3.  `E` ‒ the collision energy;
4.  `C` ‒ scale coefficient of dE/dx, should be used if dE/dx has been multiplied by this value during the reconstruction process (if in doubt, put 1);
5.  `generator` ‒ the model which has been used in simulation, possible expressions are `"LAQGSM"` (`"QGSM"`), `"URQMD"`, `"NSIG"` (model-independent n-sigma method) and `"DEFAULT"` (the “average” value, set by default);
6.  `tracking` ‒ can be `"HP"` (Hit Producer) and `"CF"` (Cluster Finder, used by default);
7.  `nSigPart` ‒ string of particles which are used in n-sigma method, possible expressions are `"el"`, `"mu"`, `"pi"`, `"ka"`, `"pr"`, `"de"`, `"tr"`, `"he3"`, `"he4"` or their combinations.

For example:

```
MpdPid *pid = new MpdPid(4.0, 4.0, 11.0, 1.0, "URQMD", "CF", "pikapr");
```

<a name="MpdPid_2"></a>
### 1.2 How to set protons/antiprotons ratio

In order to improve PID quality user may set proton/antiproton ratio corresponding to the current data set before analysis. It can be done using the following function:

```
void MpdPid::SetPrRat(Double_t PrRat)
```

Default value is `102.11`.

<a name="MpdPid_3"></a>
### 1.3 Probability vector

Probability vector is assigned to the track as PID result. Each element contains the probability to be one of the specie.

<a name="MpdPid_3_1"></a>
#### 1.3.1 How to fill probability vector

To fill the probability vector, use `FillProbs` function for every particular track. It can be done with different arguments:

```
Bool_t MpdPid::FillProbs(MpdTrack *track);
Bool_t MpdPid::FillProbs(MpdTrack *track, Double_t dedx);
Bool_t MpdPid::FillProbs(Double_t p, Double_t dedx, Int_t charge);
Bool_t MpdPid::FillProbs(Double_t p, Double_t dedx, Double_t m2, Int_t charge);

```

Function returns `kTRUE` if probability vector is successfully filled, otherwise `kFALSE`.

There is a TPC-TOF mismatch effect, the situation when track in TPC is associated with TOF hit produced by another particle. This effect is significant in low momenta. In this situation dE/dx value is reconstructed correctly, but m<sup>2</sup> value is far from expected. In that case PID cannot assign a specie to the track, thus `FillProbs` function return `kFALSE`. Nevertheless there is a possibility to fill probability vector using only dE/dx value and ignoring m<sup>2</sup> (e.g. using `FillProbs(p, dedx, charge)` function). The suggestion is to do it only for low-momenta particles.

<a name="MpdPid_3_2"></a>
#### 1.3.2 How to get probabilities

To get probabilities assigned to the species, use the following functions:
```
Double_t MpdPid::GetProbEl(void);
Double_t MpdPid::GetProbMu(void);
Double_t MpdPid::GetProbPi(void);
Double_t MpdPid::GetProbPr(void);
Double_t MpdPid::GetProbKa(void);
Double_t MpdPid::GetProbDe(void);
Double_t MpdPid::GetProbTr(void);
Double_t MpdPid::GetProbHe3(void);
Double_t MpdPid::GetProbHe4(void);
```

Each function returns the probability normalized to 1.

<a name="MpdPid_3_3"></a>
#### 1.3.3 Getting the most probable pdg value

Function `Long_t MpdPid::GetMaxProb()` returns the most probable PDG-code (including charge information). It may be called when `MpdPid::FillProbs(...)` has already called.

<a name="MpdPid_3_4"></a>
#### 1.3.4 Example

The following code illustrates how `MpdPid` can be used in external macro:
```
#include <MpdPid.h>

Double_t dedx, kfP, m2, pion, electron, kaon, proton, maxprob, fProbCut = 0.6;
Int_t charge, pidpdg;
Bool_t matchingExists, PIDresult;
/// inside of event- and kalmanTrack-loops
dedx = kalmanTrack→GetPartID();
kfP = kalmanTrack→Momentum();
charge = kalmanTrack→Charge();
/// After cuts
if (matchingExists)
{
  m2 = mpdTOFMatchingData→GetMass2();
  PIDresult = pid->FillProbs(kfP, dedx, m2, charge);
  if (  (!PIDresult) && (kfP < 0.8) )
  {
    PIDresult = pid->FillProbs(kfP, dedx, charge);
    if (!PIDresult) continue;
  }
  pion = pid->GetProbPi();
  electron = pid->GetProbEl();
  proton = pid→GetProbPr();
  kaon = pid→GetProbKa();
  Double_t Probs[] = {0, electron, pion, kaon, proton};
  maxprob =  TMath::MaxElement(5, Probs);
  if (maxprob < fProbCut) continue;
  pidpdg = pid→GetMaxProb();
}
else
{
/// Do the same using FillProbs(kfP, dedx, charge)...
}
```

<a name="MpdPid_4"></a>
### 1.4 Track selection criteria

The following track selection criteria have been applied to the data set in the stage of PID tuning:

1.  Primary tracks (from GEANT)
2.  `nHits` \> 19
3.  |eta| < 1.6
4.  TPC edge cut

TPC edge cut: if track is propagating close to TPC sector boundary, correct charge collection and momentum reconstruction are difficult. Thus the following criterion has been suggested: if 50% of track hits (or more) are closer than 1.5 cm to the sector boundary – remove this track. Suggested criterion removes ~ 4% tracks from the data.

How to use it:

```
if (TPCKalmanTrack→GetRecoQuality()) continue;
```

or

```
if (MPDTrack→GetEdgeCut()) continue;
```

Default parameters of `GetRecoQuality` function can be changed:

```
Double_t dist = 1.4, percentage = 0.6;
if (TPCKalmanTrack→GetRecoQuality(dist,percentage)) continue;
```

<a name="MpdPidQA"></a>
2\. MpdPidQA class
------------------

`MpdPid` class is tuned to the mass production data set (LAQGSM, Au+Au, 11 GeV/c). In order to tune it to another one, `MpdPidQA` class can be used.

<a name="MpdPidQA_1"></a>
### 2.1 Constructor with arguments

Constructor of `MpdPidQA` class has 7 arguments. They are the same as `MpdPid` constructor's arguments except of `nSigmaPart`.

`nSigPart` – string of particles which are used in n-sigma method and **string of particles, which will be checked for the PID quality.** Constructor

```
Double_t sigM = 4.0, sigE = 4.0, energy = 11.0, koef = 1.;
TString generator = “LAQGSM”, tracking = “CF”, nSigPart = “pikaprde”;
MpdPidQA *pidQA = new MpdPidQA(sigM, sigE, energy, koef, generator, tracking, nSigPart);
```

creates the element of `MpdPidQA` class to check PID quality for pions, kaons, protons and deuterons.

<a name="MpdPidQA_2"></a>
### 2.2 Usage
<a name="MpdPidQA_2_1"></a>
#### 2.2.1 How to fill QA histograms

`MpdPidQA` class consists of the set of histograms. Quality assurance can be achieved after these histograms have been filled. To fill histograms which are responsible to dE/dx QA use:

```
void MpdPidQA::FillDedxHists(Double_t p, Double_t dedx, Int_t realPDG);
```

To m2 QA use:

```
void MpdPidQA::Fillm2Hists(Double_t p, Double_t m2, Int_t realPDG);
```

To check the particle yields use:

```
void MpdPidQA::FillAmplHists(Double_t p, Int_t realPDG);
```

And finally to fill efficiency and contamination histograms use:

```
Bool_t MpdPidQA::FillEffContHists(MpdTrack *track, Int_t realPDG, Double_t probCut);
Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Int_t charge, Int_t realPDG, Double_t probCut);
Bool_t MpdPidQA::FillEffContHists(Double_t p, Double_t dedx, Double_t m2, Int_t charge, Int_t realPDG, Double_t probCut);
```

where `probCut` is the minimal value of probability for the particle identification (`probCut = 0` means that the particle specie is defined as maximum probability).

`FillEffContHists` returns the same value as `FillProbs` function with the same parameters. But `FillEffContHists` returns `kFALSE` in case when the maximum probability is less than `probCut`.

These functions should be called in track loop for each particular track in event.

<a name="MpdPidQA_2_2"></a>
#### 2.2.2 How to save results

In order to save `MpdPidQA` results use the following functions:

```
void MpdPidQA::GetDedxQA("/path/to/some/folder/");
void MpdPidQA::Getm2QA("/path/to/some/folder/");
void MpdPidQA::GetAmplQA("/path/to/some/folder/");
void MpdPidQA::GetEffContQA("/path/to/some/folder/", "pika", "prde", “”);
```

The last function has 3 additional parameters (type `TString`). It can be used to separate efficiency and contamination plots to the several groups of particles.

<a name="MpdPidQA_2_3"></a>
#### 2.2.3 Example

The following code illustrates how `MpdPidQA` can be used:

```
#include <MpdPidQA.h>
Double_t sigm = 4., sige = 4., energy = 11., koef = 1.;
TString Generator = "LAQGSM", Tracking = "CF";
MpdPidQA *pidQA = new MpdPidQA(sigm, sige, energy, koef, Generator, Tracking, "pikaprde");

Int_t nKalmanTracks, mcNtracks, mcTrackId, pidFlag, pdg, pdgc, nofHits, mother, charge, kfID, matchingIndex, nTofPoints, nEtofPoints, nECTTracks;
Double_t kfP, mcP, dedx, eta, Theta, m2;
Bool_t found;

Int_t nentries = chain.GetEntries(); // how much entries

FairMCTrack *mctrack = new FairMCTrack;
MpdTpcKalmanTrack* kftrack = new MpdTpcKalmanTrack;
MpdKalmanTrack* pKFtrack = new MpdKalmanTrack;
MpdTofMatchingData* Matching = new MpdTofMatchingData;

for (Int_t i=0; i<nentries; i++)
{
	chain.GetEntry(i); // current entry

	mcNtracks = mcTracks->GetEntries();
	nKalmanTracks = kalmanTracks->GetEntries();
	nTofPoints = matching->GetEntries();

	for (Int_t j=0; j<nKalmanTracks; j++)
	{
		pKFtrack = (MpdKalmanTrack*) kalmanTracks->UncheckedAt(j);
		mcTrackId = pKFtrack->GetTrackID();

		mctrack = (FairMCTrack*) mcTracks->At(mcTrackId);
		kftrack = (MpdTpcKalmanTrack*) kalmanTracks->UncheckedAt(j);

		kfP = pKFtrack->Momentum();
		mcP = mctrack->GetP();
		pdg=mctrack->GetPdgCode(); pdgc=TMath::Abs(pdg);
		Theta = TMath::PiOver2() - pKFtrack->GetParam(3);
		eta = -TMath::Log(TMath::Tan(0.5*Theta));
		mother = mctrack->GetMotherId();
		nofHits = pKFtrack->GetNofHits();
		charge = pKFtrack->Charge();

		// SETTING CUTS

		if (mother != -1) continue;
		if (nofHits < 20) continue;
		if (TMath::Abs(eta)>=1.6) continue;
		if (kftrack->GetRecoQuality()) continue;

		dedx = pKFtrack->GetPartID();
		pidQA->FillDedxHists(kfP, dedx, pdg);
		pidQA->FillAmplHists(kfP, pdg);

		found = kFALSE; m2 = 0.;

		for (Int_t l=0; l<nTofPoints; l++)
		{
			Matching = (MpdTofMatchingData*) matching->At(l);
			kfID = Matching->GetKFTrackIndex();
			if (kfID == j) {found = kTRUE; matchingIndex=l; break;}
		}
		if (found)
		{
			Matching = (MpdTofMatchingData*) matching->At(matchingIndex);
			m2 = Matching->GetMass2();
		}
		if (!found) continue;

		pidQA->Fillm2Hists(kfP, m2, pdg);
		pidQA->FillEffContHists(kfP, dedx, m2, charge, pdg, 0.);
	}
}
pidQA->GetDedxQA("/home/alex/OUT/PID/QA/");
pidQA->Getm2QA("/home/alex/OUT/PID/QA/");
pidQA->GetAmplQA("/home/alex/OUT/PID/QA/");
pidQA->GetEffContQA("/home/alex/OUT/PID/QA/", "pika", "prde");
```

<a name="MpdPidQA_3"></a>
### 2.3 Interpretation of results

`GetDedxQA` function saves graphs in choosen directory with titles:

dEdXQA__ParticleName_.C_

There are graphs illustrated the ratio of dE/dx value in asymmetric gaussian peak over dE/dx value expected from Bethe-Bloch function. It is used for estimating the parameterization quality. The closer the ratio to one the better the Bethe-Bloch description. Error bars show dE/dx resolution. _NOTE_: light nuclei parameterizations were done for the particles with 0.6 GeV/c < p < 3 GeV/c.

`Getm2QA` function saves histograms in choosen directory with titles:

m2QA_ParticleName_.C

Dots on the plot -- m2 resolution from data set, function shows the parameterization.

`GetAmplQA` function saves graphs illustrated particles yields as a function of the full momentum (with corresponded parameterization). It saves distributions with titles:

ampl_ParticleCharge_.C (i.e. amplKaNeg.C)

In case you see the following output:

   >> Positron's multiplicity normalization is incorrect!

it means that particles yields parameterization cannot be compared in correct way with the current data set. It happens due to the absence of multiplicity parameterization for the current generator/energy/tracking configuration.

`GetAmplQA` function calculate protons/antiprotons ratio and shows it as output in terminal. If value from data set and default value are different, use `MpdPid::SetPrRat` to fix it.

`GetEffContQA` function saves efficiency and contamination plots with titles:

(i.e.) pika\_poscharged\_eff.C

In case when some parameterizations are not quite good for user, it can be corrected by the hand. All parameterizations are defined in `MpdPid::Init` function.
