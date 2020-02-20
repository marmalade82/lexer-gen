with Ada.Containers.Vectors;

package Lexer_Types is
   
   type Token is ( Normal_Header, Error_Header, Default_Header,
                   Token_Name, Default_Name, Regex, Semicolon,
                   Message, Comment
                 );

   type Char_Stream is interface;
   function Has_Next(Self: Char_Stream) return Boolean is abstract;
   function Next(Self: in out Char_Stream) return Character is abstract;
   
   package P_Tokens is new Ada.Containers.Vectors 
     ( Index_Type => Natural,
       Element_Type => Token
      );
   
   subtype Tokens is P_Tokens.Vector;
   
   procedure Iter(The_Tokens: Tokens; The_Proc : not null access procedure(The_Token: Token));
   

end Lexer_Types;
