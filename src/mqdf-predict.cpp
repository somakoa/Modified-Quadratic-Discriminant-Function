#include "mqdf.h"
#include "sub.h"


#ifdef _OPENMP
#include <omp.h>
#endif

using namespace std;
using namespace Eigen;


int main(int argc, char **argv){
    
    string read_path = argv[1];

    float alpha = 0.1;
    unsigned int k = 0;
    string output_file = "result.txt";


    int op;
    while (( op = getopt(argc, argv, "o:a:k:"))  != -1){
	switch (op) {
	case 'a':
	    alpha = atof(optarg);
	    break;
	case 'k':
	    k = atoi(optarg);
	    break;
	case 'o':
	    output_file = optarg;
	    break;
	default:
	    cerr << "Usage: EXE -p power -k eigenvector_number -a alpha etc..."
		 << endl; //めんどい　後で書く
	    return -1;
	    break;
	}
    }
    
    MQDF mqdf(k, alpha);
    
    argc -= optind - 1;
    argv += optind - 1;

    const char *dir_name = argv[1];
    const char *predict_file = argv[2];

    cout << "loading dictionary ..." << endl << dir_name << endl;
    loadDictionaryDirectry(&mqdf, dir_name);


    vector<unsigned int> label_test;
    vector<Eigen::VectorXd> features_test;

    cout << "loading predict datafile ..." << endl << predict_file << endl;
    loadPredictFile(predict_file, mqdf.getDimension(), label_test, features_test);


    /*********************************************************/
    
    
    ofstream output(output_file);

    
    // read file


    /*********************************************************/

    vector<unsigned int> result_label(label_test.size()); 
    vector<double> similarity(label_test.size());
			      
    for ( unsigned int i = 0; i < label_test.size(); i++ ){
	mqdf.classify(features_test[i], result_label[i], similarity[i]);
    }


    output << "number,\tcorrect,result,\tsimilarity" << endl;
    for ( unsigned int i = 0; i < label_test.size(); i++ ){

	output << i << ",\t" <<  label_test[i] << ",\t"<< result_label[i] << ",\t" << similarity[i] << endl;
	    
    }
    
    return 0;
}




/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////




void loadDictionaryDirectry(MQDF* mqdf, const char* dir_name){

    DIR *dp;    
    struct dirent* entry;

    if(( dp = opendir(dir_name)) == NULL) {
	fprintf(stderr, "cannnot open directry : %s\n", dir_name);
	exit(1);
    }

    while(( entry = readdir(dp) ) != NULL ) {
	
	if(strcmp(".", entry->d_name) == 0 || strcmp("..", entry->d_name) == 0)
	    continue;

	string dir_path = dir_name;
	string file_name = entry->d_name;
	dir_path = dir_path + "/" + file_name;

	mqdf->loadDictionary( dir_path.c_str() );
	
    }

}



void loadPredictFile(const char* dir_name, unsigned int dimension,
		     std::vector<unsigned int> &label,
		     std::vector<Eigen::VectorXd> &features) {
    
    ifstream ifs(dir_name);
    if(ifs.fail()){
	cerr << "cannot open File : " << dir_name << endl;
	exit(2);
    }

    while(ifs.good()){
        string line;
	getline(ifs, line, '\n');
        if(line.empty())
            break;
	
        stringstream ss(line);
	int buf;
	ss >> buf;
	label.push_back(buf);
	
        string tuple;
        vector<int> idx;
	vector<double> val_buf;
        while(ss>>tuple)
	    {
		if(tuple.empty())
		    break;
		int _idx;
		double _val;
		sscanf(tuple.c_str(), "%d:%lf", &_idx, &_val);
		idx.push_back(_idx-1);
		val_buf.push_back(_val);
	    }
	
        vector<double> val(dimension, 0);
	
	for (unsigned int i = 0; i < idx.size(); i++ ){
	    val[idx[i]] = val_buf[i];
	}
	
	Eigen::VectorXd feature = Eigen::Map<Eigen::VectorXd>(&val[0], val.size());
	
        features.push_back(feature);

    }

    ifs.close();
    
    
    
    
}
