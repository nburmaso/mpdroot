/**
 *  \class MpdMcArrays
 *  \brief Holds array number of arrays, their names and sizes
 *
 *  The MpdMcArrays class holds array names, number of arrays and
 *  array default sizes
 */

#ifndef MpdMcArrays_h
#define MpdMcArrays_h

//________________
class MpdMcArrays {
 public:
  /// Default constructor
  MpdMcArrays();
  /// Destructor
  ~MpdMcArrays();

  /// Number of used arrays
  enum { NAllMpdMcArrays = 2 };

  /// Names of the TBranches in the TTree/TFile
  static const char* mcArrayNames[NAllMpdMcArrays];

  /// Names of the classes, the TClonesArrays are arrays of this type
  static const char* mcArrayTypes[NAllMpdMcArrays];

  /// Maximum sizes of the TClonesArrays
  static int mcArraySizes[NAllMpdMcArrays];

  /// Setup type indices
  enum typeIndex{ Event=0, Particle };
};

#endif // MpdMcArrays_h
