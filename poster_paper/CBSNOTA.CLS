%% 
%% This is file `CBSRep.cls'
%% Permission is granted to copy this file to another file with a clearly 
%% different name and to customize the declarations in that copy to serve 
%% the needs of your installation, provided that you comply with 
%% the conditions in the file legal.txt. 
%% 
%% However, NO PERMISSION is granted to generate or to distribute a 
%% modified version of this file under its original name. 
%% 
%% You are NOT ALLOWED to change this file. 
%% 
%% 
%% MODIFICATION ADVICE: 
%% 
%% If you want to customize this file, it is best to make a copy of the 
%% source file(s) from which it was produced. Use a different name for 
%% your copy(ies) and modify the copy(ies); this will ensure that your 
%% modifications do not get overwritten when you install a new release of 
%% the standard system. You should also ensure that your modified source 
%% file does not generate any modified file with the same name as a 
%% standard file. You can then easily distribute your modifications by 
%% distributing the modified and renamed copy of the source file, taking 
%% care to observe the conditions in legal.txt; this will ensure that 
%% other users can safely use your modifications. 
%% 
%% 
%% 
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{CBSRep}
              [1996/06/19 v1.0]
\newif\if@rdutch
\@rdutchfalse
\newif\if@rnota
\@rnotafalse
\newif\if@rrap
\@rrapfalse
\newif\if@rrapport
\@rrapportfalse
\newif\if@rreport
\@rreportfalse
\newif\if@rdisclaimer
\@rdisclaimertrue
\newif\if@rnodisclaimer
\@rnodisclaimerfalse
\DeclareOption{nota}{\@rnotatrue} % \PassOptionsToClass{oneside}{article}}
\DeclareOption{rapport}{\@rrapporttrue} % \PassOptionsToClass{twoside}{article}}
\DeclareOption{report}{\@rreporttrue} % \PassOptionsToClass{twoside}{article}}
\DeclareOption{nodisclaimer}{\@rnodisclaimertrue}
\DeclareOption{disclaimer}{\@rnodisclaimerfalse}
\DeclareOption{10pt}{\PassOptionsToClass{10pt}{article}}
\DeclareOption{11pt}{\PassOptionsToClass{11pt}{article}}
\DeclareOption{12pt}{\PassOptionsToClass{12pt}{article}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ExecuteOptions{11pt,rapport}
\ProcessOptions
\LoadClass[a4paper]{article}
\if@rnota%
  \@titlepagefalse
  \@rdutchfalse  %\@rdutchtrue		
  \@rrapfalse
  \@rrapportfalse
  \@rreportfalse
  \@rdisclaimerfalse
  \@rnodisclaimertrue
\fi
\if@rreport%
  \@titlepagetrue
  \@rdutchfalse
  \@rraptrue
  \@rnotafalse
  \@rrapportfalse
  \@rdisclaimertrue
\fi
\if@rrapport%
  \@titlepagetrue
  \@rdutchtrue
  \@rraptrue
  \@rnotafalse
  \@rreportfalse
  \@rdisclaimertrue
\fi
\if@rdutch%
%  \RequirePackage[dutch]{babel}	% commented out to allow multilanguage documents
  \frenchspacing
\fi
\newcommand{\@disclaimer}{}
\newcommand{\disclaimer}[1]{%
  \@rdisclaimertrue
  \if@rdutch%
  \renewcommand{\@disclaimer}{%
  De in dit rapport weergegeven opvattingen 
  zijn die van de auteur#1 en komen niet noodzakelijk
  overeen met het beleid van het Centraal Bureau voor
  de Statistiek.}
  \else
  \renewcommand{\@disclaimer}{%
  The views expressed in this paper are those of the author#1
  and do not necessarily reflect the policies of
  Statistics Netherlands.}
  \fi
}
\disclaimer{}
\def\nthanks#1{\footnotemark[0]%
    \protected@xdef\@thanks{\@thanks
        \protect\footnotetext[0]{#1}}%
}

\if@rnodisclaimer%
  \@rdisclaimerfalse
\fi 
\reversemarginpar
\setlength{\marginparsep}{1cm}
\newcommand*{\@respap}{}
\newcommand*{\respap}[1]{\renewcommand*{\@respap}{#1}}
\newcommand*{\@bpano}{}
\newcommand*{\bpano}[1]{\renewcommand*{\@bpano}{#1}}
\newcommand*{\@projno}{}
\newcommand*{\projno}[1]{\renewcommand*{\@projno}{#1}}
\newcommand*{\CBSname}{Centraal Bureau voor de Statistiek}
\newcommand*{\CBSDivisionname}{Divisie Methodologie en Kwaliteit}
\newcommand*{\CBSSectorname}{Sector Methodologie Heerlen}
\newcommand*{\CBSPOBoxname}{P.O. Box 4000}
\newcommand*{\CBSCityname}{2270 JM Voorburg}
\newcommand*{\Countryname}{The Netherlands}
\newcommand*{\Bpanoname}{BPA nummer}
\newcommand*{\Respapname}{Research Paper}
\newcommand*{\Projname}{Project nummer}
\newcommand*{\Datumname}{Datum}
\if@rdutch%
\renewcommand*{\CBSname}{Centraal Bureau voor de Statistiek}
\renewcommand*{\CBSDivisionname}{Divisie Technologie en Methodologie}
\renewcommand*{\CBSSectorname}{Sector Methoden en Ontwikkeling}
\renewcommand*{\CBSDivisionname}{Divisie Methodologie en Kwaliteit}
\renewcommand*{\CBSSectorname}{Sector Methodologie Heerlen}
\renewcommand*{\CBSPOBoxname}{Postbus 4000}
\renewcommand*{\Countryname}{}
\renewcommand*{\Bpanoname}{BPA-nummer}
\renewcommand*{\Projname}{Projectnummer}
\renewcommand*{\Datumname}{Datum}
\renewcommand*{\refname}{Literatuur}
\fi
\newcommand*{\@rfootprojno}{}  
\newcommand*{\rfootprojno}[1]{\renewcommand*{\@rfootprojno}{#1}}  
\newcommand*{\@rfootrespap}{}  
\newcommand*{\rfootrespap}[1]{\renewcommand*{\@rfootrespap}{#1}}  
\newcommand*{\@rfootbpano}{}  
\newcommand*{\rfootbpano}[1]{\renewcommand*{\@rfootbpano}{#1}}  
\newcommand*{\@rfootdate}{}  
\newcommand*{\rfootdate}[1]{\renewcommand*{\@rfootdate}{#1}}  
\newcommand*{\@rheadsector}{}  
\newcommand*{\rheadsector}[1]{\renewcommand*{\@rheadsector}{#1}}  
\newcommand*{\@rheaddivision}{}  
\newcommand*{\rheaddivision}[1]{\renewcommand*{\@rheaddivision}{#1}}  
%definitie van \line uit latex, met additionele lijnlengte
\def\@rline(#1,#2)#3{\@xarg #1\relax \@yarg #2\relax
  \@linelen #3\unitlength    
  \advance\@linelen by \@halfwidth  % deze regel toegevoegd
  \ifdim\@linelen<\z@\@badlinearg\else
    \ifnum\@xarg =\z@ \@vline
      \else \ifnum\@yarg =\z@ \@hline \else \@sline\fi
    \fi
  \fi}
\newcommand*{\CBSlogos}[2]{
  \setlength{\unitlength}{#1}
  \noindent
  \begin{picture}(9.8,15)
  \thicklines
  \linethickness{#2}
  \put(0,0){\@rline(1,0){9.8}}
  \put(9.8,0){\@rline(0,1){4.2}}
  \put(9.8,4.2){\@rline(-1,0){8.4}}
  \put(1.4,4.2){\@rline(0,1){1.4}}
  \put(1.4,5.6){\@rline(1,0){8.4}}
  \put(9.8,5.6){\@rline(0,1){1.4}}
  \put(9.8,7){\@rline(-1,0){9.8}}
  \put(0,7){\@rline(0,-1){4.2}}
  \put(0,2.8){\@rline(1,0){8.4}}
  \put(8.4,2.8){\@rline(0,-1){1.4}}
  \put(8.4,1.4){\@rline(-1,0){8.4}}
  \put(0,1.4){\@rline(0,-1){1.4}}
  \put(0,8){\@rline(1,0){4.2}}
  \put(4.2,8){\@rline(0,1){1.4}}
  \put(4.2,9.4){\@rline(-1,0){2.8}}
  \put(1.4,9.4){\@rline(0,1){1.4}}
  \put(1.4,10.8){\@rline(1,0){2.8}}
  \put(4.2,10.8){\@rline(0,1){1.4}}
  \put(4.2,12.2){\@rline(-1,0){4.2}}
  \put(0,12.2){\@rline(0,-1){4.2}}
  \put(5.6,8){\@rline(1,0){4.2}}
  \put(9.8,8){\@rline(0,1){4.2}}
  \put(9.8,12.2){\@rline(-1,0){2.8}}
  \put(7.0,12.2){\@rline(0,1){2.8}}
  \put(7.0,15){\@rline(-1,0){1.4}}
  \put(5.6,15){\@rline(0,-1){7}}
  \put(7.0,9.4){\@rline(1,0){1.4}}
  \put(8.4,9.4){\@rline(0,1){1.4}}
  \put(8.4,10.8){\@rline(-1,0){1.4}}
  \put(7,10.8){\@rline(0,-1){1.4}}
  \end{picture}
  \noindent
}  % einde \CBSlogos
\newcommand*{\CBSlogo}{\CBSlogos{.1cm}{.5mm}}
\newcommand*{\textwidthrule}{\rule{\textwidth}{0.4pt}}
\newlength{\@rfootheight}
\setlength{\@rfootheight}{1cm}
\newcommand*{\margintext}[1]{\makebox[0mm][r]{#1\hspace{1cm}}}
\newcommand{\ps@CBShead}{%
  \renewcommand{\@oddfoot}{%
  \parbox[b][\@rfootheight][t]{\textwidth}{%
    \normalfont
    \footnotesize \vspace{0.5ex}
    \textwidthrule 
    \\
    \begin{itshape}
    \margintext{\Projname:}%
      \@projno \hfill \@rfootprojno \\
    \margintext{\Bpanoname:}%
      \@bpano \hfill \@rfootbpano \\
    \ifx\@date\empty \date{\today} \fi
    \margintext{\Datumname:}%
      \@date \hfill \@rfootdate
    \end{itshape}
    \normalsize
}   % end \parbox
}   % end \@oddfoot
  \renewcommand{\@evenfoot}{\@oddfoot}
  \renewcommand{\@oddhead}{}
  \renewcommand{\@evenhead}{\@oddhead}
}  %end \ps@cbshead
\if@titlepage
  \renewcommand\maketitle{\begin{titlepage}%
  \thispagestyle{CBShead}
% \let\footnotesize\small
  \renewcommand{\footnoterule}{}  %CBS 
  \renewcommand\thefootnote{\@fnsymbol\c@footnote}%CBS
  \marginpar{\hfill\CBSlogo}%
  \vskip 1cm
  \noindent
  \begin{sffamily}%
  {\Large \bfseries \CBSname}
  \vspace{5pt}%
  \\
  {\large \CBSDivisionname \hfill \@rheaddivision}
  \\
  {\CBSSectorname \hfill \@rheadsector}
  \vspace{5pt}%
  \\
  \normalsize
  \begin{slshape}%
  \begin{small}%
  \CBSPOBoxname
  \\
  \CBSCityname
  \\
  \if@rdutch%
  \else
  \Countryname
  \\
  \fi
  \end{small}
  \end{slshape}
  \end{sffamily}
  \textwidthrule
  \vskip 108pt
  \begin{center}%
    \begin{bfseries}
    {\Large \sffamily \@title \par}%
    \vskip 36pt%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \end{bfseries}
  \end{center}\par
  \if@rdisclaimer%
    \nthanks{\@disclaimer}
  \fi
  \@thanks
  \end{titlepage}%
  \setcounter{footnote}{0}%
  %\let\thanks\relax%
  \renewcommand*{\thanks}[1]{}
  \let\maketitle\relax
  \gdef\@thanks{}
  \gdef\@author{}%\gdef\@title{}
  \addtolength{\textheight}{\@rfootheight}
  \addtolength{\footskip}{-\@rfootheight}
}
\else
\renewcommand\maketitle{\par
  \begingroup
    \thispagestyle{CBShead}
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    
    \marginpar{\hfill\CBSlogo}%
    \vskip 1cm
    \noindent
    \begin{sffamily}%
    {\Large \bfseries \CBSname}
    \vspace{5pt}%
    \\
    {\large \CBSDivisionname \hfill \@rheaddivision}
    \\
    {\CBSSectorname \hfill \@rheadsector}
    \vspace{5pt}%
    \\
    \normalsize

    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      %\newpage
      %\global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \if@rdisclaimer%
      \nthanks{\@disclaimer}
    \fi
    \@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}
  \addtolength{\textheight}{\@rfootheight}
  \addtolength{\footskip}{-\@rfootheight}
}
\def\@maketitle{%
  \thispagestyle{CBShead}
  %\newpage
  \null
  \vskip 2em%
  \begin{center}%
    {\Large \scshape \@title \par}%
    \vskip 1.5em%
    {\large
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
  \end{center}%
  \par
  \vskip 1.5em}
\fi
\setcounter{secnumdepth}{2}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\itshape}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\itshape}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize}}
\newenvironment{ninquote} % quotation without paragraph indents
               {\list{}{\listparindent 0em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item[]}
               {\endlist}
\renewenvironment{abstract}{%
      \vspace{12pt}
      \begin{itshape}
      \noindent
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \small
        \begin{center}%
          {\Large \scshape \rmfamily \@title \par}%
          \vspace{12pt}
%          {\bfseries \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \ninquote
      \fi}
      {\if@twocolumn\else\endquotation\fi 
       \end{itshape}\par}
%\renewcommand{\abstract}[1]{\begin{abstract} #1 \end{abstract}}
\renewenvironment{titlepage}
    {%
      \renewcommand{\baselinestretch}{1.0}
      \small\normalsize
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \if@compatibility
        \setcounter{page}{0}
      \else
        \setcounter{page}{1}%
      \fi}%
    {\if@restonecol\twocolumn 
     \else 
       \addtolength{\textheight}{\@rfootheight}  
       \addtolength{\footskip}{-\@rfootheight}   
       \newpage 
       \thispagestyle{plain}    
      \fi
      \setcounter{page}{1}%
      \renewcommand{\baselinestretch}{1.25}
      \small\normalsize
    }
\newcounter{appendix}  
\setcounter{appendix}{-1}  
\renewcommand\appendix{\clearpage %was: \par
  \stepcounter{appendix}  
  \setcounter{section}{\value{appendix}}%was: 0
  \setcounter{subsection}{0}%
  \renewcommand\thesection{\protect\appendixname\protect~ \@Alph\c@section}%\appendixname toegevoegd
  \renewcommand\thesubsection{\@Alph\c@section.\@arabic\c@subsection}
  \setcounter{equation}{0}%CBS
  \renewcommand{\theequation}{\mbox{\@Alph\c@section.\arabic{equation}}}%CBS
}
\long\def\@makefntext#1{%
    \parindent 1em%
    \noindent
    \@makefnmark#1}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{#1. #2}%
  \ifdim \wd\@tempboxa >\hsize
    #1. #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\def\@seccntformat#1{\csname the#1\endcsname\ }
\newenvironment{hanging}{\begin{list}{}
  {\setlength{\itemindent}{-\leftmargin}
  \setlength{\listparindent}{-\leftmargin}
  } \item[]}
{\end{list}}
\newenvironment{references}
{\clearpage\renewcommand{\bibitem}[1]{\item}
\section*{\refname}\begin{hanging}}
{\end{hanging}}
\renewcommand{\baselinestretch}{1.25}
\setlength{\hoffset}{-1in}
\setlength{\voffset}{-1in}
\setlength{\oddsidemargin}{4.8cm}
\setlength{\evensidemargin}{\oddsidemargin}
\if@twoside
\setlength{\evensidemargin}{3cm}
\fi
\setlength{\textwidth}{13.2cm}
\setlength{\topmargin}{1cm}
\setlength{\headheight}{0.5cm}
\setlength{\headsep}{1cm}
\setlength{\textheight}{25.2cm}
\addtolength{\textheight}{-\@rfootheight}   % ruimte voor voettekst op 1e bladzijde
\setlength{\footskip}{1.25cm}
\addtolength{\footskip}{\@rfootheight}      % ruimte voor voettekst op 1e bladzijde
\small\normalsize
\endinput
%% 
%% End of file `CBSRep.cls'.

