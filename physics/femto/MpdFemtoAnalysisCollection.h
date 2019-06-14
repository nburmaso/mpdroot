/**
 * \class MpdFemtoAnalysisCollection
 * \brief Holds a collection of analyses
 *
 * The class holds a collection (STL list) of analyses
 */

#ifndef MpdFemtoAnalysisCollection_h
#define MpdFemtoAnalysisCollection_h

// C++ headers
#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

// Forward declaration
class MpdFemtoBaseAnalysis;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<MpdFemtoBaseAnalysis*, allocator<MpdFemtoBaseAnalysis*> > MpdFemtoAnalysisCollection;
typedef list<MpdFemtoBaseAnalysis*, allocator<MpdFemtoBaseAnalysis*> >::iterator MpdFemtoAnalysisIterator;
#else
typedef list<MpdFemtoBaseAnalysis*> MpdFemtoAnalysisCollection;
typedef list<MpdFemtoBaseAnalysis*>::iterator MpdFemtoAnalysisIterator;
#endif

#endif // #define MpdFemtoAnalysisCollection_h
