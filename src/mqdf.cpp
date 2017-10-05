#include "mqdf.h"


using namespace std;
using namespace Eigen;



void MQDF::train_vv(std::vector<std::vector<double> > _features, int in_label) {

    unsigned long int samples = _features.size();
    unsigned int dim = _features[0].size();
    
    Eigen::MatrixXd features(samples, dim);
    
    for ( unsigned int i = 0; i < _features.size(); i++){
	Eigen::VectorXd tmp = Eigen::Map<Eigen::VectorXd>(&_features[i][0], _features[i].size());
	features.row(i) = tmp;
    }
	
    train(features, in_label, dim, samples);

}



void MQDF::train(Eigen::MatrixXd features, int in_label, unsigned int _dimension, unsigned long int samples){


    if ( _dimension != dimension && dimension != 0){
	cout << "found multi number about dimension" << endl;
	exit(-1);
    }
    else if (dimension == 0)
	dimension = _dimension;


    // change of variable
    features.row(0) = features.row(0).array().pow(power);

    
    // calculate mean vector
    Eigen::VectorXd MV = features.colwise().mean();

    
    // calculate covariance matrix
    Eigen::MatrixXd CM = Eigen::MatrixXd::Zero(_dimension, _dimension);
    for ( unsigned int i = 0; i < samples; i++){
	CM += (features.row(i).transpose() - MV)
	    * (features.row(i).transpose() - MV).transpose();
    }


    // calculate eigen values, eigen vectors
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(CM);
    if (es.info() != Eigen::Success) abort();


    Eigen::VectorXd EVal(_dimension);
    Eigen::MatrixXd EVec(_dimension, _dimension);

    for (unsigned int di = 0; di < _dimension; ++di) {
	EVal.coeffRef(di) = es.eigenvalues().coeff(_dimension - di - 1);
	EVec.col(di) = es.eigenvectors().col(_dimension-di-1);
    }

    unsigned int _k = EVal.size();
    
    if ( _k < max_k )
	max_k = _k;
    
    labels.push_back(in_label);
    mean_vectors.push_back(MV);
    covariance_matrix.push_back(CM);
    eigen_values.push_back(EVal);
    eigen_vectors.push_back(EVec);
    
    num_category ++;

    
}



void MQDF::saveDictionarySet(const char* dir_name){

    for( unsigned int i = 0; i < num_category; i++ ){
	
	ostringstream ss;
	ss << labels[i];
	string dic_file = string(dir_name) + "/" + ss.str() + ".dic";
	
	saveDictionary(dic_file.c_str(), labels[i]);	
    }

}

void MQDF::saveDictionary(const char* file_name, int _label){

    unsigned label_num = 0;
    for ( unsigned int i = 0; i < labels.size(); i++ ){
	if(_label == labels[i]){
	    label_num = i;
	    break;
	}
    }

    
    ofstream ofs(file_name);
    ofs << "power:\n" << power << endl;
    ofs << "max_k:\n" << max_k << endl;
    ofs << "label:\n" << labels[ label_num ] << endl;
    ofs << "dimension:\n" << dimension << endl;
    ofs << "MeanVector:\n" << mean_vectors[ label_num ].transpose() << endl;
    ofs << "ConvarianceMatrix:\n" << covariance_matrix[ label_num ] << endl;
    ofs << "EigenValues:\n" << eigen_values[ label_num ].transpose() << endl;
    ofs << "EigenVectors:\n" << eigen_vectors[ label_num ] << endl;

}

void MQDF::loadDictionary(const char* dir_name){

    ifstream ifs(dir_name);
    if(ifs.fail()){
	cout << "cannot open file : " << dir_name << endl;
	exit(-1);
    }

    string line;
    unsigned int dim;
    while ( getline(ifs, line) ){
	
	if( line == "power:" ) {
	    getline(ifs, line);
	    float  _power = atof(line.c_str());
	    if ( _power != power && num_category != 0){
		cout << "found multi number about power value" << endl;
		exit(-1);
	    }
	    power = _power;
	}

	if( line == "max_k:" ) {
	    getline(ifs, line);
	    unsigned  _k = atoi(line.c_str());
	    if (_k < max_k){
		max_k = _k;
	    }
	}

	
	if( line == "label:" ) {
	    getline(ifs, line);
	    int label = atoi(line.c_str());
	    for ( unsigned int i = 0; i < labels.size(); i++ ){
		if(labels[i] == label){
		    cout << "class " << label << " is already exist" << endl;
		    exit(-1);
		}
	    }
	    labels.push_back(label);
	}

	
	if( line == "dimension:" ){
	    getline(ifs, line);
	    dim = atoi(line.c_str());
	    if( dim != dimension && dimension != 0){
		cout << "found multi number about dimension" << endl;
		exit(-1);
	    }
	    else if (dimension == 0){
		dimension = dim;
		// K = dimension;
	    }
	    //dimensions.push_back(dim);
	}

	
	if( line == "MeanVector:" ){
	    getline(ifs, line);
	    stringstream ss(line);  
	    string tuple;
	    vector<double> val_buf;
	    
	    while(ss >> tuple){
		if(tuple.empty())
		    break;
		double _val;
		sscanf(tuple.c_str(), "%lf", &_val);
		val_buf.push_back(_val);
	    }
	    
	    Eigen::VectorXd val = Eigen::Map<Eigen::VectorXd>(&val_buf[0], val_buf.size());
	    mean_vectors.push_back(val);
	}

	
	if( line == "CovarianceMatrix:" ){
	    vector< vector<double> > cm;
	    
	    for( unsigned int i = 0; i < dim; i++ ){
		getline(ifs, line);
		stringstream ss(line);
		string tuple;
		vector<double> cm_buf;
		
		while(ss >> tuple) {
		    if(tuple.empty())
			break;
		    if(cm_buf.size() >= dim){
			cout << "load error in ConvarianceMatrix" << endl;
			exit(-1);
		    }
		    double _val;
		    sscanf(tuple.c_str(), "%lf", &_val);
		    cm_buf.push_back(_val);
		}
		
		cm.push_back(cm_buf);
		
	    }
	    Eigen::MatrixXd cm_sub(dimension, dimension);
	    
	    for ( unsigned int i = 0; i < cm.size(); i++){
		Eigen::VectorXd tmp = Eigen::Map<Eigen::VectorXd>(&cm[i][0], cm[i].size());
	        cm_sub.row(i) = tmp;
	    }

	    covariance_matrix.push_back(cm_sub);
	}

	
	if( line == "EigenValues:" ){
	    getline(ifs, line);
	    stringstream ss(line);  
	    string tuple;
	    vector<double> val_buf;
	    
	    while(ss >> tuple){
		if(tuple.empty())
		    break;
		double _val;
		sscanf(tuple.c_str(), "%lf", &_val);
		val_buf.push_back(_val);
	    }
	    
	    Eigen::VectorXd val = Eigen::Map<Eigen::VectorXd>(&val_buf[0], val_buf.size());
	    eigen_values.push_back(val);
	}
	

	if( line == "EigenVectors:" ){
	    vector< vector<double> > ev;
	    
	    for( unsigned int i = 0; i < dim; i++ ){
		getline(ifs, line);
		stringstream ss(line);
		string tuple;
		vector<double> ev_buf;
		
		while(ss >> tuple) {
		    if(tuple.empty())
			break;
		    if(ev_buf.size() >= dim){
			cout << "load error in ConvarianceMatrix" << endl;
			exit(-1);
		    }
		    double _val;
		    sscanf(tuple.c_str(), "%lf", &_val);
		    ev_buf.push_back(_val);
		}
		
		ev.push_back(ev_buf);
		
	    }

	    Eigen::MatrixXd X(dim, dim);
	    
	    for ( unsigned int i = 0; i < dim; i++){
		Eigen::VectorXd tmp = Eigen::Map<Eigen::VectorXd>(&ev[i][0], ev[i].size());
		X.row(i) = tmp;
	    }
	    
	    eigen_vectors.push_back(X);

	}
    }
    
    num_category ++;


}




//蔵元さんのコピペ　つかってません
/******************************************************************************
******************************************************************************/
void MQDF::readDictionary(std::string dir_name) {

    std::ifstream ifs( dir_name.c_str(), std::ios::in | std::ios::binary);
    if (!ifs) {
	std::cerr << "Can'n open mqdf dictionaey file." << dir_name << std::endl;
    }

    // read headers
    ifs.read((char*)&dimension, sizeof(unsigned int));
    ifs.read((char*)&num_category, sizeof(unsigned int));
    ifs.read((char*)&power, sizeof(float));
    ifs.read((char*)&max_k, sizeof(unsigned short));

    if (max_k < K) {
	std::cerr << "Can't use eigenvalues over " << max_k << "." << std::endl;
	std::cerr << "Please set the K at less than " << max_k << "." << std::endl;
    }

    // reserve data field
    codes.resize(num_category);
    mean_vectors.resize(num_category);
    eigen_values.resize(num_category);
    eigen_vectors.resize(num_category);

    // read mean vector, eigenvalues and eigenvectors
    for (unsigned int ci = 0; ci < num_category; ++ci) {
	Eigen::VectorXf Mf(dimension);
	Eigen::VectorXf Df(dimension);
	Eigen::MatrixXf Vf(dimension,dimension);
	ifs.read((char*)&codes[ci], sizeof(unsigned short));
	ifs.read((char*) Mf.data(), sizeof(float) * dimension);
	ifs.read((char*) Df.data(), sizeof(float) * dimension);
	ifs.read((char*) Vf.data(), sizeof(float) * dimension * max_k);

	// convert float to double
	mean_vectors[ci] = Mf.cast<double>();
	eigen_values[ci]  = Df.cast<double>();
	eigen_vectors[ci] = Vf.cast<double>();
    }


}
/******************************************************************************
 ******************************************************************************/






double MQDF::function(Eigen::VectorXd &X, unsigned int idx) {
    
    double first_term = (X - mean_vectors[idx]).squaredNorm();
    double second_term = 0;
    double third_term = 0;

    
    for (unsigned int j = 0; j < K; j++){
	second_term += (((1 - ALPHA) * eigen_values[idx].coeff(j)) / 
			((1 - ALPHA) * eigen_values[idx].coeff(j) + ALPHA * sigma)) *
	    (eigen_vectors[idx].col(j).transpose() * (X - mean_vectors[idx])) * 
	    (eigen_vectors[idx].col(j).transpose() * (X - mean_vectors[idx]));
	third_term += log((1 - ALPHA) * eigen_values[idx].coeff(j) + ALPHA * sigma);

    }
    //cout << first_term << " - " << second_term << " = " << first_term - second_term << "\t\t" << third_term << endl;
    //cout << "sigma : " << sigma << "  ALPHA : " << ALPHA << endl;
    return (1.0 / (ALPHA * sigma)) * (first_term - second_term) + third_term;

}

void MQDF::classify(Eigen::VectorXd X, unsigned int &_class, double &similarity) {


    // calculate sigma
    sigma = 0;
    for(unsigned int ci = 0; ci < num_category; ci++){
    	sigma += eigen_values[ci].mean();
    }
    sigma /= num_category;

    
    // change of variable
    X = X.array().pow(power);

    
    std::vector<double> values(num_category);

#ifdef _OPENMP
#pragma omp parallel for
#endif
    for (unsigned int ci = 0; ci < num_category; ci++ ) {
	
	sigma = 0;
	for(unsigned int j = K; j < dimension; j++){
	    sigma += eigen_values[ci].coeff(j);
	}
	sigma = sigma / (dimension - K);
	//cout << sigma << endl;

//	sigma = eigen_values[ci].coeff(K);
	
	values[ci] = function(X, ci);
    } 


    //     ( min_iter <- min_values_address )
    // cout << endl;
    // for (unsigned int i = 0; i < values.size(); i++){
    // 	cout << values[i] << "\t";
    // }
    // cout << endl << endl;;
	
    auto min_iter = std::min_element(values.begin(), values.end());

    
    unsigned int category = std::distance(values.begin(), min_iter);
    _class = labels[category];
    similarity = values[category];
    
}


void MQDF::setPow(float _power){

    if (num_category != 0){
	cerr << "cannot change power" << endl;
	exit(1);
    }

    power = _power;

}
