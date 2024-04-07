//shift implementes as I type
//load store are SPECIAL I type

#include<iostream>
#include <fstream>
#include<string>
#include <sstream>
#include <iomanip>
#include<cmath>
#include<bitset>

#define mmp 64      //max memory positions
#define lbl_max 64  //max label positions

using namespace std;

void print_monitors(bool endd);
int lbl_trans(string label);

string mipsRegisters[33] = {
        "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
        "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
        "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
        "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra", "-"};

int reg[33];

long memAddress[mmp];
long MEM[mmp];

string label_nm[lbl_max];
int LBL[lbl_max];
int label_count=0;

int PC=0;
bool EOF_flag=0;

int cyc_to_print[100];
int cyc_count=0;

//control signals
bool regdst, jump, branch, memread, memtoreg, memwrite, ALUsrc, regwrite, zero;
int aluop;

//monitors
int /*PC, rs, rt*/ wr, write_data, readA, readB, aluout, target, mem_in, mem_out;
string instr, opcode, type/*R I J SPI B*/;

string file_name;
int rs, rt, rd, address, immed, cycles=1;

int hex_dec(string hexString) {
    int decimalValue;
    istringstream(hexString) >> hex >> decimalValue;
    return decimalValue;
}

string intToHex(int value) {
    stringstream stream;

    // Ensure the value fits within 32 bits (4 bytes)
    value &= 0xFFFFFFFF;

    // Convert to hexadecimal
    stream << hex << uppercase << setw(8) << setfill('0') << value;

    return stream.str();
}

string intToBinary(unsigned int num) {
    // Ensure the input number is within the range [0, 15]
    num = num & 0xF;

    // Convert the integer to its binary representation
    bitset<4> binary(num);

    // Convert the binary representation to a string
    return binary.to_string();
}

string trim(string in){ //removes 0s from numbers
    int l=in.length();
    bool found=0;
    string out="";

    for(int i=0; i<l; i++){
        if(in[i]=='0' && !found){
            continue;
        }else{
            if(in[i]!='0' || found){
                found=1;
                out+=in[i];
            }
        }
    }

    if(out=="")
        out="0";

    return out;
}

int stringToInt(const string& str) {
    try {
        return stoi(str);
    } catch (const invalid_argument& e) {
        cerr << "Invalid argument: " << e.what() << endl;
        return 0;
    } catch (const out_of_range& e) {
        cerr << "Out of range: " << e.what() << endl;
        return 0;
    }
}

unsigned int directToInt(string hex) {  //converts HEX to int without considering 2s complement
    unsigned int sum=0;

    for(int i=0; i<8; i++){
        switch(hex[i]){
            case 'F':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'E':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case 'D':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'C':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-2));
            break;

            case 'B':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case 'A':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '9':
                sum+=pow(2, (((8-i)*4)-1));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '8':
                sum+=pow(2, (((8-i)*4)-1));
            break;

            case '7':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '6':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '5':
                sum+=pow(2, (((8-i)*4)-2));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '4':
                sum+=pow(2, (((8-i)*4)-2));
            break;

            case '3':
                sum+=pow(2, (((8-i)*4)-3));
                sum+=pow(2, (((8-i)*4)-4));
            break;

            case '2':
                sum+=pow(2, (((8-i)*4)-3));
            break;

            case '1':
                sum+=pow(2, (((8-i)*4)-4));
            break;
        }
    }

    return sum;
}

string parser(int line){    //returns requested line from file
    ifstream fl;
    string com="";

    EOF_flag=false;
    fl.open(file_name);
	if (!fl.is_open()) {
		exit(-1);
	}
	//FILE OK

    int j=1;
	while (j<=line) {
        if (!getline(fl, com)) {
            EOF_flag=true;
            break; // Reached the end of the file
        }
        j++;
    }

    fl.close();
    return com;
}

string formater(string inp){ //removes tabs, new lines, and spaces
	int len=inp.length();
	string out="";

	for(int i=0; i<len; i++){
		if(inp[i]=='#'){
			break;
		}else{
			if(inp[i]!=' ' && inp[i]!='\t' && inp[i]!='\n'){
				out+=inp[i];
			}
		}
	}

    //cout<<endl<<out;
	return out;
}

string deformater(string in){   //adds back spaces (eg. add$t1,$t2,$t3 to add $t1, $t2, $t3)
    string out="";

    if(in[0]=='j'){
        out+='j';
        out+=' ';
        for(int i=1; i<in.length(); i++){
            out+=in[i];
        }
        return out;
    }

    if(opcode=="add" || opcode=="addu"  || opcode=="and" || opcode=="nor" || opcode=="or" || opcode=="slt" || opcode=="sltu" || opcode=="sub" || opcode=="subu"){ //R-TYPE
        for(int i=0; i<in.length(); i++){
            out+=in[i];
            if(in[i+1]=='$'){
                out+=' ';
            }
        }
        return out;
    }

    if(opcode=="addi" || opcode=="addui" || opcode=="addiu" || opcode=="andi" || opcode=="ori" || opcode=="slti" || opcode=="sltiu" || opcode=="sltui" || opcode=="sll" || opcode=="srl" || opcode=="beq" || opcode=="bne"){ //I/B-TYPE
        int comma=0, dol=0;
        for(int i=0; i<in.length(); i++){
            out+=in[i];
            if(in[i+1]=='$'){
                out+=' ';
                dol++;
            }
            if(in[i]==','){
                comma++;
                if(comma==2){
                    out+=' ';
                }
            }
        }
        return out;
    }

    if(opcode=="lw" || opcode=="sw"){ //SPECIAL-I-TYPE
        int i=0;
        while(in[i+1]!='$'){
            out+=in[i];
            i++;
        }
        out+=in[i];
        i++;
        out+=' ';
        while(in[i]!=','){
            out+=in[i];
            i++;
        }
        out+=in[i];//comma
        i++;
        out+=' ';
        while(i<in.length()){
            out+=in[i];
            i++;
        }
        return out;
    }

    exit(-7);
    return NULL;
}

int starter(){ //finds the line where main starts
	int line=0;
	string com;
	string temp;

	while(1){
		com=parser(line);
		temp="";

		if(com.length() >= 5){
			for(int i=0; i<5; i++){
				temp+=com[i];
			}
			if(temp=="main:"){
				return line++;
			}
		}

		line++;
	}
}

void submit_label(string name, int pointer){ //saves label name and line pointer
    if(label_count==lbl_max){
        exit(-6);
    }

    label_nm[label_count]=name;
    LBL[label_count]=pointer;
    label_count++;
}

void preparser(){   //goes through the instruction file and removes labels and submits them to label array
    ofstream fout;
    fout.open("instr.txt");
    int i=0;
    string line;
    bool isLabel;
    int empt=0;

    if (!fout.is_open()) {
		exit(-5);
	}

    int mainline=starter();
    for(i=0; i<=mainline; i++){
        fout<<parser(i)<<endl;
    }

    do{
        isLabel=false;
        line=formater(parser(i));
        if(line!=""){
            if(line[line.length()-1]==':'){
                string lbl_bd="";
                int lbl_ln=i-empt-mainline-1-label_count;
                for(int j=0; j<line.length()-1; j++){
                    lbl_bd+=line[j];
                }
                submit_label(lbl_bd, lbl_ln);
            }else{
                fout<<line<<endl;
            }
        }else{
            empt++;
        }
        i++;
    }while(!EOF_flag);

    file_name="instr.txt";  //changes file pointer to "sanitized" instruction file
    fout.close();
}

string alu_trans(){     //translates command type to ALU_opcode
    if(type=="R" || type=="I")
        return "10";
    if(type=="B")
        return "01";
    if(type=="SPI")
        return "00";
    return "11";
}

int reg_trans(string alias) { //register translation from alias to number
    for (int i = 0; i < 32; i++) {
        if (mipsRegisters[i] == alias) {
            return i;
        }
    }
    return -1; // Return -1 if the alias is not found
}

void j_type(string com){    //processes j type commands
	string address_str="";
	string rs_str="";
    string label_str="";
    type="J";

	switch(com[1]){

		case 'a': //jal
			for(int i=3; i<com.length(); i++){
				address_str+=com[i];
			}
			reg[reg_trans("ra")]=PC; //save $ra
			opcode="jal";
		break;

		case 'r': //jr$
			for(int i=3; i<com.length(); i++){
				rs_str+=com[i];
			}
			address_str=intToHex(reg[reg_trans(rs_str)]);
			opcode="jr";
		break;

		default://j
			for(int i=1; i<com.length(); i++){
				label_str+=com[i];
			}
			opcode="j";
		break;
	}

	//rs=reg_trans(rs_str); ENABLE IF JRRRRRRRRRRRRRRRRRRR
	address=LBL[lbl_trans(label_str)];
	rs=32;
	rt=32;
	rd=32;
}

void r_type(string com){    //processes r type commands
	int i;
	string rs_str="";
	string rt_str="";
	string rd_str="";
	type="R";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rd start
	do{
		rd_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rd done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	rd=reg_trans(rd_str);
}

void b_type(string com){    //processes branch type commands
    int i;
	string rs_str="";
	string rt_str="";
	string label_str="";
	type="B";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	//label start
	do{
		label_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	target=LBL[lbl_trans(label_str)];
	//cout<<endl<<target<<endl;
	rd=32;
}

void i_type(string com){    //processes i type commands
    int i;
	string rs_str="";
	string rt_str="";
	string immed_str="";
	type="I";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;
	i++;
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rs done
	i++;
	//immed start
	do{
		immed_str+=com[i];
		i++;
	}while(i<com.length());

	rs=reg_trans(rs_str);
	rt=reg_trans(rt_str);
	if(immed_str[0]=='0' && immed_str[1]=='x'){
        string temp="";
        for(int j=2; j<=immed_str.length(); j++){
            temp+=immed_str[j];
        }
        immed=hex_dec(temp);
	}else{
        immed=stringToInt(immed_str);
	}
	rd=32;
}

void special_i_type(string com){    //processes special i type commands
    int i;
	string rs_str="";
	string rt_str="";
	string immed_str="";
	type="SPI";

	i=0;
	do{
		i++;
	}while(com[i]!='$');
	//opcode done
	i++;
	//rt start
	do{
		rt_str+=com[i];
		i++;
	}while(com[i]!=',');
	//rt done
	i++;//skip comma


	while(com[i]>='0' && com[i]<='0'){
        immed_str+=com[i];
        i++;
	}
	//immed done
	i++;//skip parentheses
	i++;//skip $
	//rs start
	do{
		rs_str+=com[i];
		i++;
	}while(com[i]!=')');

	if(rs_str=="pc"){
        rs=PC+1;
	}else{
        rs=reg_trans(rs_str);
	}
	rt=reg_trans(rt_str);
	immed=stringToInt(immed_str);
	rd=32;
}

void instr_reg(int line){   //reads command and sends it to appropriate func for execution
	string com=formater(parser(line));
	instr=com;
	opcode="";
	int i=0;

	cout<<com<<endl;

	if(com[0]=='j'){ //J-TYPE
		j_type(com);
	}else{
		i=0;
		do{
			opcode+=com[i];
			i++;
		}while(com[i]!='$');
		//i have opcode in string format;

		if(opcode=="add" || opcode=="addu"  || opcode=="and" || opcode=="nor" || opcode=="or" || opcode=="slt" || opcode=="sltu" || opcode=="sub" || opcode=="subu"){ //R-TYPE
			r_type(com);
		}else{
			if(opcode=="addi" || opcode=="addui" || opcode=="addiu" || opcode=="andi" || opcode=="ori" || opcode=="slti" || opcode=="sltiu" || opcode=="sltui" || opcode=="sll" || opcode=="srl"){ //I-TYPE
				i_type(com);
			}else{
			    if(opcode=="sw" || opcode=="lw"){ //SPECIAL-I-TYPE
                    special_i_type(com);
                }else{
                    if(opcode=="beq" || opcode=="bne"){ //B-TYPE
                        b_type(com);
                    }else{
                        exit(-2);
                    }
                }
			}
		}
	}
	//theoretically have opcode, rs, rt, rd, func, immed, address
}

int target_calc(){ //also monitor 10 ===============DEPRECATED
	//target=PC+immed;//not *4 bcs addresses go +1
	return target;
}

void control_unit(){    //sets controll signals ans increments PC

    PC++;

    if(instr=="sll$zero,$zero,0"){
        print_monitors(true);
		exit(0);
	}

	if(opcode=="add"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=2;//add
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="addi"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=2;//add
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="addui" || opcode=="addiu"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=7;//addu
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="addu"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=7;//addu
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="and"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=0;//and
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="andi"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=0;//and
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="beq"){
		regdst=0;
		jump=0;
		branch=1;
		memread=0;
		memtoreg=0;
		aluop=6;//sub
		memwrite=0;
		ALUsrc=0;
		regwrite=0;
	}

	if(opcode=="bne"){
		regdst=0;
		jump=0;
		branch=1;
		memread=0;
		memtoreg=0;
		aluop=6;//sub
		memwrite=0;
		ALUsrc=0;
		regwrite=0;
	}

	if(opcode=="lw"){
		regdst=0;
		jump=0;
		branch=0;
		memread=1;
		memtoreg=1;
		aluop=2;//add
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="nor"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=12;//nor
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="or"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=1;//or
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="ori"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=1;//or
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="slt"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=3;//slt
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="slti"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=3;//slt
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="sltiu" || opcode=="sltui"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=8;//sltu
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="sltu"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=8;//sltu
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="sll"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=5;//sll
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="srl"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=4;//srl
		memwrite=0;
		ALUsrc=1;
		regwrite=1;
	}

	if(opcode=="sw"){
		regdst=0;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=1;
		aluop=2;//add
		memwrite=1;
		ALUsrc=1;
		regwrite=0;
	}

	if(opcode=="sub"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=6;//sub
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="subu"){
		regdst=1;
		jump=0;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=9;//subu
		memwrite=0;
		ALUsrc=0;
		regwrite=1;
	}

	if(opcode=="j" || opcode=="jal"){
		regdst=0;
		jump=1;
		branch=0;
		memread=0;
		memtoreg=0;
		aluop=2;//add
		memwrite=0;
		ALUsrc=0;
		regwrite=0;
	}
}

void reg_file(){    //sets ALU inputs and saves data to registers
	readA=reg[rs];
	readB=reg[rt];
	mem_in=readB;

	if(memtoreg){
		write_data=mem_out;
	}else{
		write_data=aluout;
	}

	if(regdst){
		wr=rd;//write register
	}else{
		wr=rt;
	}

	if(regwrite){
		reg[wr]=write_data;
	}
}

void ALU(){
	int inA=readA;
	int inB;

	if(ALUsrc){
		inB=immed;
	}else{
		inB=readB;
	}

	switch(aluop){

		case 2://add
			aluout=inA+inB;
		break;

		case 6://sub
			aluout=inA-inB;
		break;

		case 3://slt
			aluout=inA-inB;
			if(aluout<0)
                aluout=1;
            else
                aluout=0;
		break;

		case 8://sltu
			aluout=directToInt(intToHex(inA))-directToInt(intToHex(inB));
			if(aluout<0)
                aluout=1;
            else
                aluout=0;
		break;

		case 0://and
			aluout=inA & inB;
		break;

		case 1://or
			aluout=inA | inB;
		break;

		case 12://nor
			aluout=~(inA | inB);
		break;

		case 5://sll
			aluout=inA;
			for(int i=0; i<inB; i++){
				aluout*=2;
			}
		break;

		case 4://srl
			aluout=inA;
			for(int i=0; i<inB; i++){
				aluout/=(int)2;
			}
		break;

		case 7://addu
            aluout=directToInt(intToHex(inA))+directToInt(intToHex(inB));
        break;

        case 9://subu
			aluout=directToInt(intToHex(inA))-directToInt(intToHex(inB));
		break;
	}

	if(aluout==0){
		zero=1;
	}else{
		zero=0;
	}
}

long msbPC(){   //returns only PC MSB ==== probably deprecated
	long msb=PC & (~(268435455));
	//cout<<endl<<msb<<endl;
	return msb;
}

void jmp(){     //saves jump address to PC
	if(jump){
		PC=address; // was addr+msbPC()
	}
}

int lbl_trans(string label){    //  translates label name to index in LBL[] array (which contains the file line pointer)
    for(int i=0; i<lbl_max; i++){
        if(label_nm[i]==label){//search for existing label
            return i;
        }
    }
    return -1;
}

int mem_trans(unsigned int addr){   //translates memory address to index in MEM[] array (which contains memory data)
    for(int i=0; i<mmp; i++){
        if(memAddress[i]==addr){//search for existing mem address
            return i;
        }
    }
    //else
    for(int i=0; i<mmp; i++){//assign new mem address
        if(memAddress[i]==0){
            memAddress[i]=addr;
            return i;
        }
    }
    exit(-3);//OUT OF MEMORY
}

void memory(){  //read/writes from/to memory based on controll signals
	if(memread){
		mem_out=MEM[mem_trans(aluout)];
	}

	if(memwrite){
		MEM[mem_trans(aluout)]=mem_in;
	}
}

void fillMem(){     //initiates registers with predetermined values
    reg[reg_trans("gp")]=hex_dec("10008000");
    reg[reg_trans("sp")]=hex_dec("7FFFFFFC");
}

void print_monitors(bool endd){     //used for debugging
    ofstream fout;
    fout.open("output.txt", ios::app);
    if (!fout.is_open()) {
		exit(-4);
	}

	if(endd){
        fout<<endl<<"-----Final State-----"<<endl;
	}else{
        fout<<endl<<"-----Cycle "<<cycles<<"-----"<<endl;
	}
	fout<<"Registers:"<<endl;
	if(endd)
        fout<<trim(intToHex(PC*4-4))<<'\t';
    else
        fout<<trim(intToHex(PC*4))<<'\t';
	for(int i=0; i<32; i++){
        if(reg[i]==0){
            fout<<"0"<<'\t';
        }else{
            fout<<trim(intToHex(reg[i]))<<'\t';
        }
	}

	if(!endd){
        fout<<endl<<endl<<"Monitors:"<<endl;
        //fout<<endl<<type<<endl;
        fout<<trim(intToHex(PC*4-4))<<'\t'<<deformater(instr)<<'\t';
        if(type!="J")fout<<"$"<<mipsRegisters[rs]<<'\t'; else fout<<"-"<<'\t';
        if(type=="R")fout<<"$"<<mipsRegisters[rt]<<'\t'; else fout<<"-"<<'\t';
        if(type=="R" || type=="I" || opcode=="lw")fout<<"$"<<mipsRegisters[wr]<<'\t'; else fout<<"-"<<'\t';
        if(type=="R" || type=="I" || type=="SPI")fout<<trim(intToHex(write_data))<<'\t'; else fout<<"-"<<'\t';
        fout<<trim(intToHex(readA))<<'\t';
        if(type!="I" && type!="SPI" && type!="J")fout<<trim(intToHex(readB))<<'\t'; else fout<<"-"<<'\t';
        fout<<trim(intToHex(aluout))<<'\t';
        if(type=="B")fout<<trim(intToHex(target))<<'\t'; else fout<<"-"<<'\t';
        if(type=="SPI")fout<<trim(intToHex(aluout))/*same as mem_address*/<<'\t'<<trim(intToHex(mem_in))<<'\t'<<trim(intToHex(mem_out))<<'\t'; else fout<<"-"<<'\t'<<"-"<<'\t'<<"-"<<'\t';
        fout<<regdst<<'\t';
        if(type=="J" || 1)fout<<jump<<'\t'; else fout<<"-"<<'\t';
        if(type=="B" || 1)fout<<branch<<'\t'; else fout<<"-"<<'\t';
        fout<<memread<<'\t'<<memtoreg<<'\t'<<alu_trans()<<'\t'<<memwrite<<'\t'<<ALUsrc<<'\t'<<regwrite;
	}

	fout<<endl<<endl<<"Memory State:"<<endl;

	if(opcode=="sw"){
        fout<<trim(intToHex(MEM[mem_trans(aluout)]));
	}

    if(endd){
        for(int i=0; i<mmp; i++){
            if(memAddress[i]!=0 && MEM[i]!=0){
                fout<<trim(intToHex(MEM[i]))<<'\t';
            }
        }
    }

    fout<<endl;
    if(endd){
        fout<<endl<<"Total Cycles:"<<endl<<cycles-1;
    }

	fout.close();
}

void monitors(){    //prints monitor values if user asked for current cycle to be printed
    if(cycles==cyc_to_print[cyc_count]){
        cyc_count++;
        print_monitors(false);
    }else{
        if(cyc_to_print[cyc_count]==0){
            print_monitors(false);
        }
    }
}

void init_out(){    //initializes output file
    ofstream fout;
    fout.open("output.txt");
    if (!fout.is_open()) {
		exit(-4);
	}
	fout<<"Name: MCH170"<<endl;
}

void brnch(){   //commits branch command if requirements are met
    //target=2;
    if(branch && zero && opcode=="beq"){
        PC=target;
    }
    if(branch && !zero && opcode=="bne"){
        PC=target;
    }
}

int main(int argc, char *argv[])//possible error: FIX OFFSET SYSTEM FOR BRANCH (probably fixed, not tested)
{
    int i=0;
	cout<<"Command file name: ";
    cin>>file_name;
    cout<<"Cycles to print in ascending order (-1 to continue, 0 for all): "<<endl;
    do{
        cin>>cyc_to_print[i];
        i++;
    }while(cyc_to_print[i-1]!=-1 && cyc_to_print[i-1]!=0);

    preparser();
    init_out();
    fillMem();
	int mainline=starter()+1;

	while(1){

		instr_reg(mainline+PC);
		control_unit();
		target_calc();
		reg_file();
		ALU();
		memory();
		reg_file();
		brnch();
		jmp();
        monitors();
        cycles++;
	}

    print_monitors(true);
    return 0;
}

//0 ALL GOOD - 1 NO FILE - 2 INVALID COMMAND - 3 OUT OF MEMORY - 4 OUT FILE ERROR - 5 PREPARSER ERROR - 6 OUT OF LABEL SPACE
