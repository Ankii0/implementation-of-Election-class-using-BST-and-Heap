
import java.io.*;
import java.util.ArrayList;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Scanner;

 class TwoDBlockMatrix{

    float[][] matrix;

    TwoDBlockMatrix(float[][] arr){
        int n=arr.length,m=arr[0].length;
        this.matrix= new float[n][m];
        this.matrix = arr.clone();
    }

     TwoDBlockMatrix multiply(TwoDBlockMatrix other) throws IncompatibleDimensionException {
        int p, q, a, b;
        p = this.matrix.length;
        q = this.matrix[0].length;
        a = other.matrix.length;
        b = other.matrix[0].length;
        if (q != a) throw new IncompatibleDimensionException("IncompatibleDimension");
        else {
            float[][] arr = new float[p][b];
            for (int i = 0; i < p; i++) {
                for (int j = 0; j < b; j++) {
                    arr[i][j] = 0;
                    for (int k = 0; k < q; k++)
                        arr[i][j] += this.matrix[i][k] * other.matrix[k][j];
                }
            }
            return new TwoDBlockMatrix(arr);
        }
    }

    TwoDBlockMatrix transpose(){
        int p,q;
        p=this.matrix.length;q=this.matrix[0].length;
        float[][] arr =new float[q][p];
        for (int j=0;j<q;j++){
            for (int i=0;i<p;i++)
                arr[j][i]=this.matrix[i][j];
        }
        return new TwoDBlockMatrix(arr);
    }

    static TwoDBlockMatrix buildTwoDBlockMatrix(InputStream obj) {
        String content_str = "";
        try {
            int content;

            while ((content = obj.read()) != -1) {
                content_str = content_str + (char) content;
            }
        }
        catch (IOException e){
            System.out.println(e);
        }

            ArrayList<Float> s = new ArrayList<Float>();
            ArrayList<Integer> row = new ArrayList<Integer>(),col =new ArrayList<Integer>();
            int x = 0, indx = 0;

            for (String data : content_str.split("\\n")) {
                String[] arr = data.split(";|\\s");
                if (arr[0].equals("#")) {
                    x = 0;
                    continue; }
                if (x == 0) {
                    x = 1;
                    if (!row.isEmpty()) {
                        row.remove(row.size() - 1);
                        s.remove(s.size() - 1);
                        col.remove(col.size() - 1); }

                    row.add(Integer.parseInt(arr[0]));
                    col.add(Integer.parseInt(arr[1]));
                    s.add(null);
                    indx = col.size() - 1;
                }
                else {
                    for (int j = 0; j < arr.length; j++) {
                        col.add(col.get(indx) + j);
                        row.add(row.get(row.size() - 1));
                        s.add(Float.parseFloat(arr[j])); }

                    row.add(row.get(row.size() - 1) + 1);
                    col.add(null);
                    s.add(null);
                }
            }

            s.remove(s.size() - 1);
            col.remove(col.size() - 1);
            row.remove(row.size() - 1);

            float[][] arr = new float[row.get(row.size() - 1)][col.get(col.size() - 1)];

            for (int i = 0; i < s.size(); i++) {
                if (s.get(i) != null) {
                    arr[row.get(i) - 1][col.get(i) - 1] = s.get(i);
                }
            }
    return new TwoDBlockMatrix(arr);}

    private float[] mult(float[] a,float b){
        int l=a.length;
        float [] res = new float[l];
        for (int i=0;i<l;i++)
            res[i]=a[i]*b;
        return res;}

    private float[] add(float[] a,float[] b ){
        int l=a.length;
        float[] res=new float[l];
        for(int i=0;i<l;i++) res[i]=a[i]+b[i];
        return res; }

    private void equal(float[] a,float[] b){
        int l=a.length;
        for(int i=0;i<l;i++)
        a[i]=b[i]; }

    private void swap(float[] a,float[] b ){
        float temp;
        int l=a.length;
        for(int i=0;i<l;i++){
            temp = a[i];
            a[i]=b[i];
            b[i]=temp; }
    }

    TwoDBlockMatrix inverse() throws InverseDoesNotExistException {
        int p,q,a;
        p=this.matrix.length;q=this.matrix[0].length;
        if (p!=q)
            throw new InverseDoesNotExistException("InverseDoesNotExist");
        else{
        float[][] l=new float[p][p];
        for(int i=0;i<p;i++)
            equal(l[i],this.matrix[i]);
        float[][] l1= new float[p][p];
        for (int i=0;i<p;i++) l1[i][i]=1;

        for(int i=0;i<p-1;i++){
            a=i;
            while (l[i][i]==0){
                if (l[a+1][i]!=0){
                    swap(l[i],l[a+1]);
                    swap(l1[i],l1[a+1]);
                }
                else if(a+1==p-1){
                    throw new InverseDoesNotExistException("InverseDoesNotExist");
                }
                else a=a+1; }

            for (int j=i+1;j<p;j++){
                float b=l[j][i]/l[i][i];
                equal(l[j],add(mult(l[i],-b),mult(l[j],1)));
                equal(l1[j],add(mult(l1[i],-b),mult(l1[j],1))); }
        }

        for(int i=p-1;i>-1;i--){
            for(int j=i-1;j>-1;j--){
                if (l[i][i]==0) throw new InverseDoesNotExistException("InverseDoesNotExist");
                else {
                float b=l[j][i]/l[i][i];
                equal(l[j],add(mult(l[i],-b),mult(l[j],1)));
                equal(l1[j],add(mult(l1[i],-b),mult(l1[j],1))); } }
        }

        for(int i=0;i<p;i++){
            equal(l1[i],mult(l1[i],1/l[i][i]));
            equal(l[i],mult(l[i],(1/l[i][i]))); }

        return new TwoDBlockMatrix(l1);}
    }
  private ArrayList<Float> copy(ArrayList<Float> arr){
        ArrayList<Float> a=new ArrayList<Float>();
        for(int i=0;i<arr.size();i++)
            a.add(arr.get(i));
        return a; }
  private ArrayList<ArrayList<Float>> copy1(ArrayList<ArrayList<Float>> arr){
        ArrayList<ArrayList<Float>> a=new ArrayList<ArrayList<Float>>();
        for(ArrayList<Float> i:arr)
            a.add(i);
        return a; }

   public String toString(){
        String s="";
        int m=this.matrix.length,n=this.matrix[0].length;
        int i=0,j=0,x=0;
        float [][] arr =new float[m][n];
        for(int k=0;k<m;k++) equal(arr[k],this.matrix[k]);
        ArrayList<Integer> pos = new ArrayList<Integer>();
        HashMap<Integer,ArrayList<ArrayList<Float>>> dir = new HashMap<Integer,ArrayList<ArrayList<Float>>>();
        ArrayList<Float> l1 = new ArrayList<Float>();
        ArrayList<ArrayList<Float>> l= new ArrayList<ArrayList<Float>>();
        while (i<m){
            l1.clear();l.clear();
            int siz=0,a;
            int b=0,for_b;
            for_b=0;
            j=0;a=0;
            while (j<n){
                if (arr[i][j]!=0 ){
                    a=1;
                    l1.add(arr[i][j]);
                    arr[i][j]=0;
                    if (for_b==0) {b=j;for_b=for_b+1;pos.add(i+1);pos.add(j+1);}
                    j=j+1;
                }
                else if((a==1)&(arr[i][j]==0)) break;
                else j=j+1;
            }
            if (l1.isEmpty()) {i=i+1; continue;}
                int len=l1.size();
                l.add(copy(l1));
                l1.clear();
                for (int k=i+1;k<m;k++){
                    int c=b;
                    while(c<b+len){
                        if (arr[k][c]!=0)
                            l1.add(arr[k][c]);
                        c=c+1;
                    }
                    if (l1.size()==len){
                        l.add(copy(l1)); siz=siz+1;
                        l1.clear();
                        c=b;
                        while(c<b+len){
                            if (arr[k][c]!=0)
                                arr[k][c]=0;
                            c=c+1; }
                    }
                    else break; }
                dir.put(x,copy1(l));
                l.clear();
                pos.add(x);
                x=x+1;
                for(j=0;j<m;j++)
                    if (arr[i][j]!=0) break;
                    else if (j==m-1) i=i+1;

            }

      DecimalFormat format2dec = new DecimalFormat("0.00");
      Iterator< Integer > it = dir.keySet().iterator ();
        for(i=0;i<pos.size();i+=3){
            s=s+format2dec.format(pos.get(i))+" "+format2dec.format(pos.get(i+1))+"\n";
            for (ArrayList<Float> o:dir.get(pos.get(i + 2))){
                for(float f:o){
                    s=s+format2dec.format(f)+" ";}

                s=s.substring(0,s.length()-1);
                s=s+";"+"\n";
            }
            s=s+"#"+"\n";
        }
      s=s.substring(0,s.length()-1);
        return s;
        }

 TwoDBlockMatrix getSubBlock(int rows,int cols, int rowe,int cole) throws SubBlockNotFoundException {
    int m = rowe - rows, n = cole - cols;
    if (m <= 0 || n <= 0) throw new SubBlockNotFoundException("SubBlockNotFound");
    else {
        float[][] arr = new float[m][n];
        for (int i = rows - 1; i < rowe - 1; i++) {
            for (int j = cols - 1; j < cole - 1; j++) {
                if (this.matrix[i][j] == 0) throw new SubBlockNotFoundException("SubBlockNotFound");
                else
                    arr[i - rows + 1][j - cols + 1] = this.matrix[i][j];
            }
        }
        return new TwoDBlockMatrix(arr);
    }
}
}
    class InverseDoesNotExistException extends Exception
    {
        public InverseDoesNotExistException(String s){
            super(s); }   }

    class IncompatibleDimensionException extends Exception {
        public IncompatibleDimensionException(String s){
            super(s); }   }

    class SubBlockNotFoundException extends Exception
    {
        public SubBlockNotFoundException(String s){
            super(s); }   }


