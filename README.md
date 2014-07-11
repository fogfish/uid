# k-ordered unique identity

The library provide interface to generate k-ordered unique id in decentralized manner for Erlang application. The concept is similar to (snow-) flake identity schema, however the library developed schema to ensure

  *  64-bit unique identity within virtual machine
  * 128-bit globally unique identity 

Client applications benefits from short 64-bit identity using it locally and dynamically switches schema while communicating with remote peers. The library is based on monotonically increasing erlang:now() feature.

## 64-bit

   ```
        24bit          20bit          20bit
   |--------------|--------------|--------------|
          A              B              C
   
   {A, B, C} = erlang:now()

   ```

## 128-bit

   ```
         24bit         20bit          20bit       16bit      48bit
   |--------------|--------------|--------------|-------|--------------|
           A             B              C          Node   MAC / Worker 
   
   Node = erlang:phash(erlang:node(), 1 bsl 16). 

   ```
   