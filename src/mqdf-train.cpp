#include "Eigen/Dense"
#include "mqdf.h"
#include "sub.h"


#ifdef _OPENMP
#include <omp.h>
#endif

using namespace std;
using namespace Eigen;


float power = 1.0;

int main(int argc, char **argv) {

    
    if(argc < 3){
	cerr << "usage: mqdf-train feature_file output_file" << endl;
	return -1;
    }


    int op;
    while (( op = getopt(argc, argv, "p:o:")) != -1){
	switch (op) {
	case 'p':
	    power = atof(optarg);
	    break;   
	default:
	    cerr << "Usage: mqdf-train -p power [input_feature_dir] [output_dictionary_dir]" << endl;
	    //ちゃんと後で書きます
	    return -1;
	}
    }


    argc -= optind - 1;
    argv += optind - 1;

    char *in_dir = argv[1];
    char *out_dir =argv[2];


    MQDF mqdf;
    mqdf.setPow(power);
    loadFeatures_Dir(in_dir, &mqdf);
    mqdf.saveDictionarySet(out_dir);
    
    return 0;
}



void loadFeatures_Dir(const char* dir_name, MQDF *mqdf){
    
    
    DIR *dp;    
    struct dirent* entry;

    if(( dp = opendir(dir_name)) == NULL) {
	fprintf(stderr, "cannot open directry : %s\n", dir_name);
	exit(1);
    }

    while(( entry = readdir(dp) ) != NULL ) {
	
	if(strcmp(".", entry->d_name) == 0 || strcmp("..", entry->d_name) == 0)
	    continue;

	string file_path = dir_name;
	string file_name = entry->d_name;
	file_path = file_path + "/" + file_name;

	cout << "make dictionary of " << file_name << endl;
	loadFeatures(file_path.c_str(), mqdf);
	
	
    }


}


void loadFeatures(const char* file_name, MQDF *mqdf){

    ifstream in_file(file_name);
    if(in_file.fail()){
	cout << "cannot found File : " << file_name << endl;
	exit(1);
    }

    vector< vector<double> > features;
    unsigned int dim = 0;
    int label;
    int _label;

    unsigned long int samples = 0;
    
    while( in_file.good() ) {
	string line;
	getline(in_file, line, '\n');
	if( line.empty() )
	    break;

	stringstream ss(line);

	ss >> label;
	if (samples == 0)
	    _label = label;
	
	if(label != _label){
	    cout << "exist multi label error in lineNumber ... " << samples << endl;
	    exit(1);
	}
	
	string tuple;
	vector<unsigned int> idx;
	vector<double> val;
	while( ss >> tuple ) {	    
	    if(tuple.empty())
		break;    
            unsigned int _idx;
            double _val;
            sscanf(tuple.c_str(), "%d:%lf", &_idx, &_val);
	    if ( dim < _idx )
		dim = _idx;
            idx.push_back(_idx-1);
	    val.push_back(_val);
	}
	
        vector<double> feature(dim, 0);
	for (unsigned int i = 0; i < idx.size(); i++ ){
	    feature[idx[i]] = val[i];
	}

	features.push_back(feature);
	
	samples ++;
    }

    for( unsigned int i = 0; i < features.size(); i++ )
	features[i].resize(dim, 0);

    
    mqdf->train_vv(features, label);


}

