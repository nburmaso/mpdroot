#include <MpdFemtoContainer.h>

//--------------------------------------------------------------------------

MpdFemtoContainer::MpdFemtoContainer() {
    fMo.SetPxPyPzE(.0, .0, .0, .0);
    fCo.SetPxPyPzE(.0, .0, .0, .0);
    fEventNumber = 0;
}

//--------------------------------------------------------------------------

MpdFemtoContainer::MpdFemtoContainer(Int_t ev, TLorentzVector mom, TLorentzVector coord) {
    fEventNumber = ev;
    fMo = mom;
    fCo = coord;
}

//--------------------------------------------------------------------------

MpdFemtoContainer::~MpdFemtoContainer() {

}