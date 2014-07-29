#include <iostream>
#include <TMath.h>
#include <sys/wait.h>
#include <cassert>
#include <TApplication.h>


void showFoundClustersQA3();

int main()
{
 std::cout << "Let's start our dance" << std::endl;
 TApplication theApp("Run", 0, 0);
 showFoundClustersQA3();
 theApp.Run();
}
