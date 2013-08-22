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
\ProvidesClass{cbsrep}[2008/04/08 v2.1, P.P. de Wolf (op basis C. Zeelenberg)]
\newif\ifkeyw@rds
\keyw@rdsfalse
\newif\if@rdutch
\@rdutchfalse
\newif\if@rnota
\@rnotafalse
\newif\if@rrap
\@rrapfalse
\newif\if@rtimes
\@rtimesfalse
\newif\if@rrapport
\@rrapportfalse
\newif\if@rreport
\@rreportfalse
\newif\if@rdisclaimer
\@rdisclaimertrue
\newif\if@rnodisclaimer
\@rnodisclaimerfalse
\newif\if@notadisclaimer
\@notadisclaimerfalse
\DeclareOption{nota}{\@rnotatrue\PassOptionsToClass{oneside}{article}}
\DeclareOption{rapport}{\@rrapporttrue\PassOptionsToClass{twoside}{article}}
\DeclareOption{report}{\@rreporttrue\PassOptionsToClass{twoside}{article}}
\DeclareOption{nodisclaimer}{\@rnodisclaimertrue}
\DeclareOption{disclaimer}{\@rnodisclaimerfalse\@notadisclaimertrue}
\DeclareOption{10pt}{\PassOptionsToClass{10pt}{article}}
\DeclareOption{11pt}{\PassOptionsToClass{11pt}{article}}
\DeclareOption{12pt}{\PassOptionsToClass{12pt}{article}}
\DeclareOption{times}{\@rtimestrue}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ExecuteOptions{11pt,rapport}
\ProcessOptions
\LoadClass[a4paper,openbib,final]{article}
\if@rnota%
  \@titlepagefalse
  \@rdutchtrue
  \@rrapfalse
  \@rrapportfalse
  \@rreportfalse
  \@rdisclaimerfalse
  \@rnodisclaimertrue
  \if@notadisclaimer
    \@rnodisclaimerfalse
    \@rdisclaimertrue
  \fi
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
%\RequirePackage{amsmath,amsthm,flafter}
%\RequirePackage[psamsfonts]{amssymb}
\if@rtimes%
   \RequirePackage{amsmath}
%  \RequirePackage{mathptmx,textcomp,times,mathtime}
%  \RequirePackage[cmbold]{mathtime}
  \RequirePackage{txfonts}
%  \RequirePackage[psamsfonts]{amssymb}
\else
%  \RequirePackage{amssymb}
   \RequirePackage{amsmath}
\fi
\AtBeginDocument{\allowdisplaybreaks[4]}
\newcommand*{\onehalf}{\ensuremath{\tfrac12}}
\newcommand*{\onequarter}{\ensuremath{\tfrac14}}
\newcommand*{\threequarters}{\ensuremath{\tfrac34}}
\if@rdutch%
\RequirePackage[english,dutch]{babel}
\AtBeginDocument{\frenchspacing}
\else
\RequirePackage[dutch,english]{babel}
\fi
% Onderstaande werkt niet meer in nieuwe pdflatex omgeving
%\newif\ifpdf
%\ifx\pdfoutput\undefined
%\pdffalse
%\else
%\pdfoutput=1
%\pdftrue
%\fi
% Daarom vervangen door:
\RequirePackage{ifpdf}

\ifpdf%
\AtEndOfClass{\RequirePackage[pdftex]{hyperref}}
\else
\AtEndOfClass{\RequirePackage[dvips]{hyperref}}
\fi

\newcommand*{\@verslagtype}{this paper }
\if@rnota\renewcommand{\@verslagtype}{deze nota }
\else
\if@rrapport\renewcommand{\@verslagtype}{dit rapport }
\fi
\fi
\newcommand{\@disclaimer}{}
\newcommand{\disclaimer}[1]{%
  \@rdisclaimertrue
  \if@rdutch%
  \renewcommand{\@disclaimer}{%
  De in \@verslagtype weergegeven opvattingen 
  zijn die van de auteur#1 en komen niet noodzakelijk
  overeen met het beleid van het Centraal Bureau voor
  de Statistiek.}
  \else
  \renewcommand{\@disclaimer}{%
  The views expressed in \@verslagtype are those of the author#1
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
\newcommand*{\@sctitle}{}
\renewcommand*{\title}[1]{\renewcommand*{\@title}{#1}
  \renewcommand{\@sctitle}{\uppercase{#1}}
\ifpdf%
\hypersetup{pdftitle=\@title,pdfsubject=\@title,pdfkeywords=\@title}
\fi
}
\renewcommand*{\author}[1]{\renewcommand*{\@author}{#1}
\ifpdf\hypersetup{pdfauthor=\@author}\fi
}

\newcommand*{\@keywords}{}
\newcommand*{\Keywords}[1]{\renewcommand*{\@keywords}{#1}}

\newcommand*{\@respap}{}
\newcommand*{\respap}[1]{\renewcommand*{\@respap}{#1}}
\newcommand*{\@bpano}{}
\newcommand*{\bpano}[1]{\renewcommand*{\@bpano}{#1}}
\newcommand*{\@projno}{}
\newcommand*{\projno}[1]{\renewcommand*{\@projno}{#1}}
\newcommand*{\CBSname}{Statistics Netherlands}

\newcommand*{\CBSDivisionname}{Process development, IT and Methodology}
\newcommand*{\Divisie}[1]{\renewcommand*{\CBSDivisionname}{#1}}

\newcommand*{\CBSSectorname}{Methodology and Development}
\newcommand*{\Sector}[1]{\renewcommand*{\CBSSectorname}{#1}}

\newcommand*{\CBSPOBoxname}{P.O. Box 4481}
\newcommand*{\CBSCityname}{6401 CZ Heerlen}
\newcommand*{\Countryname}{The Netherlands}
\newcommand*{\Bpanoname}{BPAnumber}
\newcommand*{\Respapname}{Research Paper}
\newcommand*{\Projname}{Projectnumber}
\newcommand*{\Datumname}{Date}
\newcommand*{\Keywordname}{Keywords}
\if@rdutch%
\renewcommand*{\CBSname}{Centraal Bureau voor de Statistiek}
\renewcommand*{\CBSDivisionname}{Procesontwikkeling, IT, Methodologie}
\renewcommand*{\CBSSectorname}{Methodologie en Ontwikkeling}
\renewcommand*{\CBSPOBoxname}{Postbus 4481}
\renewcommand*{\CBSCityname}{6401 CZ Heerlen}
\renewcommand*{\Countryname}{}
\renewcommand*{\Bpanoname}{BPA-nummer}
\renewcommand*{\Projname}{Projectnummer}
\renewcommand*{\Datumname}{Datum}
\renewcommand*{\Keywordname}{Trefwoorden}
\newcommand{\renamecommand}[2]{\let#2=#1}
\renamecommand{\captionsdutch}{\oldcaptionsdutch}
\renewcommand{\captionsdutch}{%
\oldcaptionsdutch
\renewcommand*{\refname}{Literatuur}}
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
\newcommand*{\@rheadcbs}{}  
\newcommand*{\rheadcbs}[1]{\renewcommand*{\@rheadcbs}{#1}}  
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
\ifpdf%
\newcommand*{\CBSlogo}{\CBSlogos{.1cm}{.5mm}}
\else
\IfFileExists{cbs}{\input{cbslogo}\newcommand*{\CBSlogo}{\setcbslogo}}
{\newcommand*{\CBSlogo}{\CBSlogos{.1cm}{.5mm}}}
\fi
%\RequirePackage{cbslogo}
\newcommand*{\textwidthrule}{\rule{\textwidth}{0.4pt}}
\newlength{\@rfootheight}
\setlength{\@rfootheight}{1cm}
\newcommand*{\margintext}[1]{\makebox[0mm][r]{#1\hspace{\marginparsep}}}
\newcommand{\ps@CBShead}{%
  \renewcommand{\@oddfoot}{%
  \begin{minipage}[b][\@rfootheight][t]{\textwidth}%
    \footnotesize \vspace{0.5ex}
    \ifx\@date\empty \date{\today} \fi
    \setlength{\tabcolsep}{0mm}
    \begin{itshape}
    \renewcommand{\arraystretch}{0.9}
    \begin{tabular*}{\textwidth}[t]{rl@{\extracolsep{\fill}}r}
    \hline
    \margintext{\Projname:}%
      & \@projno & \@rfootprojno \\
    \margintext{\Bpanoname:}%
      & \@bpano & \@rfootbpano \\
    \margintext{\Datumname:}%
      & \@date & \@rfootdate
    \end{tabular*}
    \end{itshape}
   \end{minipage}
}   % end \@oddfoot
  \renewcommand{\@evenfoot}{\@oddfoot}
  \renewcommand{\@oddhead}{}
  \renewcommand{\@evenhead}{\@oddhead}
}  %end \ps@cbshead
\newcommand*{\makehead}{%
  \noindent
  \begin{sffamily}%
  \margintext{\normalsize\CBSlogo}%
  {\Large \bfseries \CBSname \hfill \@rheadcbs}
  \vspace{5pt}%
  \newlength{\headtablen}
  \ifx\@empty\@rheaddivision %
  \setlength{\headtablen}{\textwidth}
  \else \setlength{\headtablen}{0.5\textwidth}
  \fi
  \ifx\@empty\@rheadsector %
  \else \setlength{\headtablen}{0.5\textwidth}
  \fi
  \renewcommand{\arraystretch}{0.75}
  \noindent
  \begin{tabular*}{\textwidth}{@{}p{\headtablen}%
   @{\extracolsep\fill}p{\headtablen}@{}}
  {\large \CBSDivisionname} & {\large \@rheaddivision}  \\
  \CBSSectorname & \@rheadsector \\
  \if@titlepage
  {\small \slshape \CBSPOBoxname} \\
  {\small \slshape \CBSCityname} \\
  \if@rdutch \else {\small \slshape \Countryname}  \\
  \fi
  \fi
  \hline
 \end{tabular*}
 \end{sffamily}
 \noindent%
} % \makehead
\if@titlepage
  \renewcommand\maketitle{\begin{titlepage}%
  \thispagestyle{CBShead}
  \renewcommand{\footnoterule}{}
  \renewcommand\thefootnote{\@fnsymbol\c@footnote}
  \makehead
  \par
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
%  \gdef\@author{}%\gdef\@title{}
  \addtolength{\textheight}{\@rfootheight}
  \addtolength{\footskip}{-\@rfootheight}
  \newpage\pagenumbering{roman}\setcounter{page}{2} %Bij TOC e.d. roman nummering gebruiken en bij paginanr. 2 beginnen
}
\else
\renewcommand\maketitle{\par
  \begingroup
    \thispagestyle{CBShead}
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
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
  \gdef\@thanks{}%\gdef\@author{}\gdef\@title{}
  \addtolength{\textheight}{\@rfootheight}
  \addtolength{\footskip}{-\@rfootheight}
%  \newpage\pagenumbering{roman}\setcounter{page}{2} %Bij TOC e.d. roman nummering gebruiken en bij paginanr. 2 beginnen
}
\def\@maketitle{%
  \newpage
  \null
  \makehead
  \vskip 70pt%
  \begin{center}%
    {\Large \bfseries \@sctitle \par}%
    \vskip 24pt%
    {\large \bfseries
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}
    \par}%
  \end{center}%
 }  % @maketitle
\fi
\setcounter{secnumdepth}{3}
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
\newenvironment{abstractquote} % quotation 
               {\setlength{\parindent}{0em}%
                \setlength{\parskip}{0ex plus0.1ex minus0.1ex}%
	        \list{}
                 {\listparindent 0em
                  \leftmargin 0.5cm
                  \rightmargin 0.5cm
		  \topsep 0ex plus0.1ex minus0.1ex
		  \partopsep 0ex plus0.1ex minus0.1ex
                 }
                 \item[]
                 }{\endlist}
\renewenvironment{abstract}{%
      \if@titlepage
	\newpage\thispagestyle{empty}\if@twoside \ifodd\c@page\thispagestyle{plain}\else
    	\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi\thispagestyle{plain}\fi\fi
	\pagenumbering{arabic}
	\newpage\thispagestyle{empty}\setcounter{page}{1}
        \addcontentsline{toc}{section}{\abstractname}
        \begin{center}%
          {\Large \scshape \rmfamily \@title}%
        \end{center}%
      \fi
      \begin{itshape}
      \noindent
      \pagenumbering{arabic}
      \begin{abstractquote}
      }
      {\par\Keywordname: \@keywords \end{abstractquote}\end{itshape}\par}
\newcommand{\singlespacing}{%
  \renewcommand{\baselinestretch}{1.0}\small\normalsize}
\newcommand{\doublespacing}{%
  \renewcommand{\baselinestretch}{2.0}\small\normalsize}
\newcommand{\triplespacing}{%
  \renewcommand{\baselinestretch}{3.0}\small\normalsize}
\newcommand{\standardspacing}{%
  \renewcommand{\baselinestretch}{\standardlinestretch}\small\normalsize}
\renewenvironment{titlepage}
    {%
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
  \sbox\@tempboxa{\itshape #1. #2}%
  \ifdim \wd\@tempboxa >\hsize
    {\itshape #1. #2}\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\def\@seccntformat#1{\csname the#1\endcsname\ \ }
\newenvironment{vhanging}[1]{\begin{list}{}
  {\setlength{\leftmargin}{#1}
  \setlength{\itemindent}{-\leftmargin}
  \setlength{\listparindent}{-\leftmargin}
} \item[]}
{\end{list}}
\newenvironment{hanging}
{\begin{vhanging}{1cm}}{\end{vhanging}}

\def\@biblabel#1{}

\def\bibcite{\@newl@bel b}

\def\bibitem{\@ifnextchar[\@lbibitem\@bibitem}
\def\@lbibitem[#1]#2{\item[\@biblabel{#1}\hfill]\if@filesw
      {\let\protect\noexpand
       \immediate
       \write\@auxout{\string\bibcite{#2}{#1}}}\fi\ignorespaces}
\def\@bibitem#1{\item\if@filesw \immediate\write\@auxout
       {\string\bibcite{#1}{\the\value{\@listctr}}}\fi\ignorespaces}

\def\@setcite#1#2#3{\expandafter#2#1\null}

\def\@citex[#1]#2{%
  \let\@citea\@empty
  \@cite{\@for\@citeb:=#2\do
    {\@citea\def\@citea{,\penalty\@m\ }%
     \edef\@citeb{\expandafter\@firstofone\@citeb\@empty}%
     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
     \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
       \G@refundefinedtrue
       \@latex@warning
         {Citation `\@citeb' on page \thepage \space undefined}}%
       {\def\@citeyear{\expandafter\@setcite\csname b@\@citeb\endcsname\@secondoftwo{}}%
        \def\@citename{\expandafter\@setcite\csname b@\@citeb\endcsname\@firstoftwo{}}%
	 \hbox{\@citename\ (\ignorespaces\@citeyear)}}}}{#1}}

\def\@citexNP[#1]#2{%
  \let\@citea\@empty
  \@cite{\@for\@citeb:=#2\do
    {\@citea\def\@citea{,\penalty\@m\ }%
     \edef\@citeb{\expandafter\@firstofone\@citeb\@empty}%
     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
     \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
       \G@refundefinedtrue
       \@latex@warning
         {Citation `\@citeb' on page \thepage \space undefined}}%
       {%\def\@citeyear{\expandafter\@setcite\csname b@\@citeb\endcsname\@secondoftwo{}}%
        %\def\@citename{\expandafter\@setcite\csname b@\@citeb\endcsname\@firstoftwo{}}%
        Huh?
	 \hbox{\@citename, \ignorespaces\@citeyear}}}}{#1}}

\DeclareRobustCommand\citeNP{%
  \@ifnextchar [{\@tempswatrue\@citexNP}{\@tempswafalse\@citexNP[]}}

\let\@internalcite\cite
\let\@internalciteNP\citeNP
\def\cite{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
    \def\citeauthoryear##1##2##3{##1 ##3}\@internalcite}
\def\citeNP{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
    \def\citeauthoryear##1##2##3{##1 ##3}\@internalciteNP}

\setlength{\bibindent}{1cm}

\renewenvironment{thebibliography}[1]
     {\section*{\refname}%
      \list{}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
	    \advance\leftmargin\bibindent
	    \itemindent -\bibindent
            \listparindent \itemindent}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m%
      \def\newblock{}}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\newenvironment{references} % Voor compatibiliteit met oude versie behouden
{\clearpage\renewcommand{\bibitem}[1]{\item}
\section*{\refname}\begin{vhanging}{\bibindent}}
{\end{vhanging}}

\newcommand*{\standardlinestretch}{1.25}
\renewcommand{\baselinestretch}{\standardlinestretch}
\setlength{\hoffset}{-1in}
%\setlength{\voffset}{-1in}
\setlength{\voffset}{-3.05cm}
\setlength{\oddsidemargin}{4.8cm}
\setlength{\evensidemargin}{\oddsidemargin}
\if@twoside
\setlength{\evensidemargin}{3cm}
\fi
\setlength{\parindent}{0em}
\setlength{\parskip}{6pt plus1pt minus3pt}
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
