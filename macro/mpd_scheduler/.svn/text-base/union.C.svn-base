#if !defined(__CINT__) || defined(__MAKECINT__)

// ROOT includes
#include "TSystem.h"
#include "TFile.h"
#include "TObjArray.h"
#include "TIter.h"
#include "TChain.h"
#include "TChainElement.h"

#include <iostream>
using namespace std;
#endif

//mode: 0 - save only result TChain with 'cbmsim' tree,
//      1 - merge and delete partitial files
void union(char* pcFileList, int mode = 1)
{
    // Load libraries
    gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");
    mpdloadlibs(kTRUE,kTRUE);       // all libs

    Bool_t res;
    TString sResultFile = "", sFirstFile = "";
    int i = 0, beg = 0, countFiles = 0;

    // MERGE PARTITIAL FILES
    if (mode == 1)
    {
        TFileMerger mergerFull,         // mergerFull for full merging with first file
                    mergerPart;         // mergerPart for merging only 'cbmsim' tree

        // go to the end of the string with a file list
        while (pcFileList[i] != '\0')
        {
            // if space then add next file to chain (TChain:Add)
            if ((pcFileList[i] == ' ') && (beg != i))
            {
                pcFileList[i] = '\0';

                switch (countFiles)
                {
                    // if first file name then it's result file (write to sResultFile string)
                    case 0:
                        sResultFile = &pcFileList[beg];
                        break;
                    // if second file name then it's first file to full merge with 'cbmsim' tree and other objects (e.g. FairBaseParSet)
                    case 1:
                    {
                        sFirstFile = &pcFileList[beg];
                        mergerFull.AddFile(&pcFileList[beg]);
                        break;
                    }
                    // if other then it's file to merge only 'cbmsim' tree
                    default:
                    {
                        mergerPart.AddFile(&pcFileList[beg]);
                    }
                }

                pcFileList[i] = ' ';
                beg = i+1;
                countFiles++;
            }

            i++;
        }

        // after string end has been found, add last file to chain
        if (beg != i)
        {
            switch (countFiles)
            {
                // if first file name then it's result file (write to sResultFile string)
                case 0:
                {
                    sResultFile = &pcFileList[beg];
                    break;
                }
                // if second file name then it's first file to full merge with 'cbmsim' tree and other objects (e.g. FairBaseParSet)
                case 1:
                {
                    sFirstFile = &pcFileList[beg];
                    mergerFull.AddFile(&pcFileList[beg]);
                    break;
                }
                // if other then it's file to merge only 'cbmsim' tree
                default:
                {
                    mergerPart.AddFile(&pcFileList[beg]);
                }
            }

            countFiles++;
        }

        // merge the files to the result file
        switch (countFiles)
        {
            case 0:
            {
                cout<<"Only one file name in the input string, nothing was merged"<<endl;
                return;
            }
            case 1:
            {
                res = TFile::Cp(sFirstFile, sResultFile);
                if (res == kFALSE)
                    cout<<"Error: the first file wasn't copied to the result file"<<endl;
                else
                {
                    //delete temporary file
                    if (remove(sFirstFile.Data()) != 0)
                        perror( "Error: deleting temporary file");
                    else
                        cout<<endl<<"Temporary file were successfully removed"<<endl;
                }

                break;
            }
            default:
            {
                bool isError = false;
                TString onlyList = "cbmsim";
                mergerPart.AddObjectNames(onlyList);
                mergerPart.OutputFile(sResultFile);
                res = mergerPart.PartialMerge(TFileMerger::kAll|TFileMerger::kIncremental|TFileMerger::kOnlyListed);
                if (res == kFALSE)
                    cout<<"Error: partial merging to the result file"<<endl;
                else
                {
                    //delete temporary files
                    TIter next(mergerPart.GetMergeList());
                    TObjString* url = 0;
                    while (url = (TObjString*)next())
                    {
                        if (remove(url->GetString().Data()) != 0)
                        {
                            isError = true;
                            perror( "Error: deleting temporary file");
                        }
                    }
                }
                mergerPart.Reset();

                mergerFull.AddFile(sResultFile);
                mergerFull.OutputFile(sResultFile);
                res = mergerFull.Merge();
                if (res == kFALSE)
                    cout<<"Error: full merging to the result file"<<endl;
                else
                {
                    //delete first temporary file
                    if (remove(sFirstFile.Data()) != 0)
                    {
                        isError = true;
                        perror( "Error: deleting first temporary file");
                    }
                }
                mergerFull.Reset();

                if (isError)
                    cout<<"There were errors while temporary files removing"<<endl;
                else
                    cout<<endl<<"Temporary files were successfully removed"<<endl;
            }
        }
    }
    // SAVE ONLY TCHAIN
    else
    {
        TChain chainUnion("cbmsim");

        // go to the end of the string with a file list
        while (pcFileList[i] != '\0')
        {
            // if space then add next file to chanin (TChain:Add)
            if ((pcFileList[i] == ' ') && (beg != i))
            {
                pcFileList[i] = '\0';

                // if first file name is defined then it's result file (write to sResultFile string)
                if (countFiles == 0)
                    sResultFile = &pcFileList[beg];
                else
                    chainUnion.Add(&pcFileList[beg]);

                pcFileList[i] = ' ';
                beg = i+1;
                countFiles++;
            }

            i++;
        }

        // after string end has been found, add last file to chain
        if (beg != i)
        {
            if (countFiles == 0)
                sResultFile = &pcFileList[beg];
            else
                chainUnion.Add(&pcFileList[beg]);

            countFiles++;
        }

        //write result file with TChain
        if (countFiles > 1)
        {
            TFile fChain(sResultFile, "RECREATE");
            chainUnion.Write();
            fChain.Close();

            Int_t events = chainUnion.GetEntries();
            cout<<"The Chain witn "<<events<<" event(s) was written to file \""<<sResultFile<<"\" to point following files:"<<endl;

            /*TObjArray *fileElements = chainUnion.GetListOfFiles();
            TIter next(fileElements);
            TChainElement* chEl = 0;
            while (chEl = (TChainElement*)next())
            {
                char* pc = chEl->GetTitle();
                cout<<pc<<endl;
            }*/
        }
    }

/*
    //test reading result file
    TChain chainRead("cbmsim");
    chainRead.Add(sResultFile);

    Int_t events = chainRead.GetEntries();
    cout<<"The count of events in test reading is equal "<<events<<endl;
*/
}
