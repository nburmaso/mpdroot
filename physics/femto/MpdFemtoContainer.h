#ifndef MPDFEMTOCONTAINER_H
#define MPDFEMTOCONTAINER_H 1

#include <iostream>
#include <TNamed.h>
#include <TLorentzVector.h>

using namespace std;

class MpdFemtoContainer : public TNamed {
public:
    MpdFemtoContainer();
    MpdFemtoContainer(Int_t, TLorentzVector, TLorentzVector);
    
    virtual ~MpdFemtoContainer();

    // Getters
    Int_t GetEventNumber() {
        return fEventNumber;
    }

    TLorentzVector Get4Momentum() {
        return fMo;
    }

    TLorentzVector Get4Coordinate() {
        return fCo;
    }

    // Setters
    void SetEventNumber(Int_t number) {
        fEventNumber = number;
    }

    void Set4Coordinate(TLorentzVector coord) {
        fCo = coord;
    }

    void Set4Momentum(TLorentzVector mom) {
        fMo = mom;
    }


private:

    Int_t fEventNumber;
    TLorentzVector fMo; // 4-momentum of particle
    TLorentzVector fCo; // 4-coordinate of particle

    ClassDef(MpdFemtoContainer, 1)
};

#endif