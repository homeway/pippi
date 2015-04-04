 

import java.io.UnsupportedEncodingException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Scanner;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

public class Encrypter
{
  private static byte[] b = { 
    79, 62, 93, 38, -125, -45, -70, -104, -99, -85, -33, -70, -122, -71, 109, 
    -51, -23, 121, -29, 73, -42, 118, 42, -23 };

  private static SecretKey KEY = new SecretKeySpec(b, "DESede");
  private static String CHAR_SET = "GBK";

  public static String encode(String str)
    throws NoSuchPaddingException, NoSuchAlgorithmException, InvalidKeyException, UnsupportedEncodingException, BadPaddingException, IllegalBlockSizeException, IllegalStateException
  {
    if (str == null) {
      return null;
    }

    Cipher cp = Cipher.getInstance("DESede");
    cp.init(1, KEY);

    byte[] ptext = str.getBytes(CHAR_SET);
    byte[] ctext = cp.doFinal(ptext);
    String r = getString(ctext);
    return r;
  }

  public static String decode(String str)
    throws NoSuchPaddingException, NoSuchAlgorithmException, InvalidKeyException, BadPaddingException, IllegalBlockSizeException, IllegalStateException, UnsupportedEncodingException
  {
    if (str == null) {
      return null;
    }
    Cipher cp = Cipher.getInstance("DESede");
    cp.init(2, KEY);
    byte[] ctext = getByte(str);
    byte[] strbyte = cp.doFinal(ctext);
    String r = new String(strbyte, CHAR_SET);
    return r;
  }

  private static byte[] getByte(String str)
  {
    String[] strs = str.split(",");
    byte[] bytes = new byte[strs.length];
    for (int i = 0; i < strs.length; i++) {
      bytes[i] = new Byte(strs[i]).byteValue();
    }
    return bytes;
  }

  private static String getString(byte[] bytes)
  {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < bytes.length; i++) {
      sb.append(Byte.toString(bytes[i]) + ",");
    }
    return sb.toString();
  }

  public static void main(String[] args)
    throws IllegalStateException, IllegalBlockSizeException, BadPaddingException, UnsupportedEncodingException, InvalidKeyException, NoSuchAlgorithmException, NoSuchPaddingException
  {
    String Action = args[0];
    String Str = args[1];
    if(Action.equals("encode")) {
      System.out.print(encode(Str));
    }
    else if(Action.equals("decode")) {
      System.out.print(decode(Str));
    }
    else {
      System.out.print("");
    }
    // System.out.print("请输入明文:");
    // Scanner in=new Scanner(System.in);
    // String readLine = in.nextLine();
    // String aa = readLine;//"-38,-91,15,-21,4,80,119,-64,-77,-122,71,-124,80,122,67,-124,";
    // String bb = encode(aa);
    // System.out.println("加密密码:"+bb);

  // 	System.out.print("请输入密文:");
		// Scanner in=new Scanner(System.in);
  //   String readLine = in.nextLine();
  //   String aa = readLine;//"-38,-91,15,-21,4,80,119,-64,-77,-122,71,-124,80,122,67,-124,";
  //   String bb = decode(aa);
  //   System.out.println("解密密码:"+bb);
   
  } //supervisor  dgtaian2012  supervisor
  //025 ab198382
}
