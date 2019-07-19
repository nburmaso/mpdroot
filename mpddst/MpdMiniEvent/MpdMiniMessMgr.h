/**
 * \class MpdMiniMessMgr
 * \brief Manager for messaging headers and typedefs
 *
 * Next includes and define keys allow to make a messaging
 * system in both local environment (vanilla ROOT) and MpdRoot
 *
 * \author Grigory Nigmatkulov (NRNU MEPhI)
 * \email nigmatkulov@gmail.com ; ganigmatkulov@mephi.ru
 * \date July 11, 2019
 */

#ifndef MpdMiniMessMgr_h
#define MpdMiniMessMgr_h

//#if defined(_VANILLA_ROOT_)
#include "Riostream.h"
#define LOG_INFO  std::cout
#define LOG_DEBUG std::cout
#define LOG_WARN  std::cout
#define LOG_ERROR std::cout
#define endm      std::endl
//#else
//#include "St_base/StMessMgr.h"
//#endif

#endif // #define MpdMiniMessMgr_h
