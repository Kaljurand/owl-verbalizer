<?xml version="1.0" encoding="UTF-8"?>

<!--
| OWL 1.1 XML to ACE lexicon (Prolog notation) converter
|
| Kaarel Kaljurand
| 2006-07-09
|
| Usage example:
|
| xsltproc owl11xml_to_acelexicon.xslt ~/Desktop/ontologies/Hydrology0.1.owl11xml | sort | uniq > Hydrology0.1.acelex.pl
|
| Note that sorting is needed, otherwise there are too many duplicates.
| Note that in case of the Hydrology ontology, some words will be empty atoms (because
| the URI does not contain the #-sign)
| and some words contain a dot ('.'). Those entries must be removed, otherwise
| the lexicon checker rejects them (or rejects the whole lexicon?), in ACE, a word
| cannot contain a dot.
|
| In general, we reject names which contain unsupported symbols.
| Unsupported symbols are those that ACE uses in a special function and
| thus splits off from words (e.g. ., ?, etc), or those that are simply
| not supported by the tokenizer (@, *, etc). The rejection rules in this
| file are complete (with regards to ASCII printable characters).
-->

<xsl:stylesheet
      xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
      xmlns:owl11xml="http://www.w3.org/2006/12/owl11-xml#"
      version="1.0"
>

<xsl:output method="text"/>

<!-- Note: we delete the prefix (until the hash).
In general it is not a good idea,
as it removes the difference between
variables from different namespaces. -->

<xsl:template match="owl11xml:OWLClass|owl11xml:ObjectProperty|owl11xml:Individual|owl11xml:DataProperty">
	<xsl:variable name="name"><xsl:value-of select="substring-after(@URI, '#')"/></xsl:variable>

	<xsl:choose>
		<xsl:when test="@URI = 'http://www.w3.org/2002/07/owl#Thing'"/>
		<!-- This happens if the URI contains no hash or ends with a hash. -->
		<xsl:when test="$name = ''">
			<xsl:text>/* ERROR: not supported: empty name */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '.')">
			<xsl:text>/* ERROR: illegal character . in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '?')">
			<xsl:text>/* ERROR: illegal character ? in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '!')">
			<xsl:text>/* ERROR: illegal character ! in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, ':')">
			<xsl:text>/* ERROR: illegal character : in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '\')">
			<xsl:text>/* ERROR: illegal character \ in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '/')">
			<xsl:text>/* ERROR: illegal character / in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, ',')">
			<xsl:text>/* ERROR: illegal character , in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '%')">
			<xsl:text>/* ERROR: illegal character % in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '*')">
			<xsl:text>/* ERROR: illegal character * in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '#')">
			<xsl:text>/* ERROR: illegal character # in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '|')">
			<xsl:text>/* ERROR: illegal character | in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '`')">
			<xsl:text>/* ERROR: illegal character ` in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '(')">
			<xsl:text>/* ERROR: illegal character ( in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, ')')">
			<xsl:text>/* ERROR: illegal character ) in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '+')">
			<xsl:text>/* ERROR: illegal character + in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, ';')">
			<xsl:text>/* ERROR: illegal character ; in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '=')">
			<xsl:text>/* ERROR: illegal character = in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '@')">
			<xsl:text>/* ERROR: illegal character @ in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '[')">
			<xsl:text>/* ERROR: illegal character [ in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, ']')">
			<xsl:text>/* ERROR: illegal character ] in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '^')">
			<xsl:text>/* ERROR: illegal character ^ in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '{')">
			<xsl:text>/* ERROR: illegal character { in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '}')">
			<xsl:text>/* ERROR: illegal character } in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '~')">
			<xsl:text>/* ERROR: illegal character ~ in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '&quot;')">
			<xsl:text>/* ERROR: illegal character " in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test='contains($name, "&apos;")'>
			<xsl:text>/* ERROR: illegal character &apos; in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '&amp;')">
			<xsl:text>/* ERROR: illegal character &amp; in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '&lt;')">
			<xsl:text>/* ERROR: illegal character &lt; in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>
		<xsl:when test="contains($name, '&gt;')">
			<xsl:text>/* ERROR: illegal character &gt; in </xsl:text>
			<xsl:value-of select='$name'/>
			<xsl:text> */&#10;</xsl:text>
		</xsl:when>

		<xsl:when test="local-name() = 'OWLClass'">
			<xsl:call-template name="count_noun">
				<xsl:with-param name="name" select="$name"/>
			</xsl:call-template>
		</xsl:when>
		<xsl:when test="local-name() = 'ObjectProperty'">
			<xsl:call-template name="tr_verb">
				<xsl:with-param name="name" select="$name"/>
			</xsl:call-template>
		</xsl:when>
		<!-- DataProperty names are added as both verbs and nouns. -->
		<xsl:when test="local-name() = 'DataProperty'">
			<xsl:call-template name="tr_verb">
				<xsl:with-param name="name" select="$name"/>
			</xsl:call-template>
			<xsl:call-template name="count_noun">
				<xsl:with-param name="name" select="$name"/>
			</xsl:call-template>
		</xsl:when>
		<xsl:when test="local-name() = 'Individual'">
			<xsl:call-template name="propername">
				<xsl:with-param name="name" select="$name"/>
			</xsl:call-template>
		</xsl:when>
		<xsl:otherwise>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="text()"></xsl:template>


<xsl:template name="count_noun">
	<xsl:param name="name"/>

	<xsl:text>count_noun(</xsl:text>
	<xsl:text>singular('</xsl:text>
	<xsl:value-of select='$name'/>
	<xsl:text>'), </xsl:text>
	<xsl:text>plural('</xsl:text>
	<xsl:value-of select='$name'/>
	<xsl:text>'), </xsl:text>
	<xsl:text>gender(neutr)</xsl:text>
	<xsl:text>).&#10;</xsl:text>
</xsl:template>


<xsl:template name="tr_verb">
	<xsl:param name="name"/>

	<xsl:text>tr_verb(</xsl:text>
	<xsl:text>third_singular('</xsl:text>
		<xsl:value-of select='$name'/>
	<xsl:text>'), </xsl:text>
	<xsl:text>third_plural('</xsl:text>
		<xsl:value-of select='$name'/>
	<xsl:text>'), </xsl:text>
	<xsl:text>past_participle('</xsl:text>
		<xsl:value-of select='$name'/>
	<xsl:text>')</xsl:text>
	<xsl:text>).&#10;</xsl:text>
</xsl:template>


<xsl:template name="propername">
	<xsl:param name="name"/>

	<xsl:text>propername(</xsl:text>
	<xsl:text>name('</xsl:text>
		<xsl:value-of select='$name'/>
	<xsl:text>'), </xsl:text>
	<xsl:text>number(singular), </xsl:text>
	<xsl:text>gender(neutr)</xsl:text>
	<xsl:text>).&#10;</xsl:text>
</xsl:template>

</xsl:stylesheet>
