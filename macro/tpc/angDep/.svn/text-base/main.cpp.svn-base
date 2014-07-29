#include <iostream>
#include <TMath.h>
#include <sys/wait.h>
#include <cassert>
#include "parameters.hpp"
#include "TStopwatch.h"

const Double_t pi = TMath::Pi();

void runMCTrack (const MCParameters& p);
void tpcSimulation(const MCParameters& p);

pid_t forkNewMCProcess(const MCParameters& p);
pid_t forkTpcSimProcess(const MCParameters& p);

void multiprocess(int nProcessors, const std::vector<MCParameters> &ps, 
                        pid_t (*pFun)(const MCParameters&));

int mainTheta();
int mainPhi();

int main()
{  
 TStopwatch timer;
  timer.Start();

  int res = mainPhi();
  
  timer.Stop();
  Double_t rtime = timer.RealTime();
  Double_t ctime = timer.CpuTime();
  printf("RealTime=%f seconds, CpuTime=%f seconds\n",rtime,ctime);

 return res;
}

int mainTheta()
{
 std::vector<MCParameters> ps;

 for(int i = 0; i < 2*6; i++)
 {
  char name[200];
  sprintf(name, "dataTheta%02i", i);

  MCParameters p;
  p._name = std::string(name);
  p._phi = 0; 
  p._theta = pi/2 + i*pi/36;
  p._nEvents = 100;
  p._shift = TVector3(0, 0, 10);
  ps.push_back(p);
 }

 const int nProcessors = 4;
 multiprocess(nProcessors, ps, forkNewMCProcess);
 multiprocess(nProcessors, ps, forkTpcSimProcess);

}

int mainPhi()
{
 std::vector<MCParameters> ps;

 for(int i = 0; i < 2*6; i++)
 {
  char name[200];
  sprintf(name, "data%02i", i);

  MCParameters p;
  p._name = std::string(name);
  p._phi = i*pi/72;
  p._theta = pi/2;
  p._nEvents = 100;
  p._shift = TVector3(0, 0, 10);
  ps.push_back(p);
 }

 const int nProcessors = 4;
 multiprocess(nProcessors, ps, forkNewMCProcess);
 multiprocess(nProcessors, ps, forkTpcSimProcess);

}

void multiprocess(int nMaxProcess, const std::vector<MCParameters> &ps, 
                        pid_t (*pFun)(const MCParameters&))
{
 for(int i = 0; i < ps.size(); i+=nMaxProcess)
 {
  int nProcess = ( i + nMaxProcess > ps.size() ) ? ps.size() - i : nMaxProcess;

  int status[nProcess];
  pid_t pids[nProcess];
  for(int j = 0; j < nProcess; j++)
     pids[j] = pFun( ps.at(i + j) );
  for(int j = 0; j < nProcess; j++)
    waitpid(pids[j], &(status[j]), 0);
 }
}

int forkTpcSimProcess(const MCParameters& p)
{
 pid_t id = fork();

 if (id == -1)
 {
   std::cout << "Can't create child process";
   return 0;
 }
 
 if  (id )  //in Parent process
 {
   return id;
 }
 else     // in Child process
 {
  tpcSimulation(p);
  exit(0);
 }
 
 assert( true ); //Must not be here
}

int forkNewMCProcess(const MCParameters& p)
{
 pid_t id = fork();

 if (id == -1)
 {
   std::cout << "Can't create child process";
   return 0;
 }
 
 if  (id )  //in Parent process
 {
    return id;
 }
 else     // in Child process
 {
  runMCTrack(p);
  exit(0);
 }
 
 assert( true ); //Must not be here
}