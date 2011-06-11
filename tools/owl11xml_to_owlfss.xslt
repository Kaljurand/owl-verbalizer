<?xml version="1.0" encoding="UTF-8"?>

<!--
| Simple OWL 1.1 XML to OWL 1.1 Functional-Style Syntax (Prolog notation) converter
|
| Kaarel Kaljurand
| 2006-10-15
|
| TODO:
| * Should the imports be handled here? No, this is just a converter to OWLFSS.
| * Escape apos and slash properly
-->

<xsl:stylesheet
      xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
      xmlns:owl11xml="http://www.w3.org/2006/12/owl11-xml#"
      version="1.0"
>

<xsl:output method="text"/>

<!-- Parsing the Ontology, which is a list of axioms. -->
<xsl:template match="owl11xml:Ontology">
	<xsl:text>'Ontology'(</xsl:text>

	<xsl:choose>
		<xsl:when test="@URI = ''">
			<xsl:text>''</xsl:text>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>'</xsl:text>
			<xsl:value-of select='translate(@URI, "&apos;", "`")'/>
			<xsl:text>'</xsl:text>
		</xsl:otherwise>
	</xsl:choose>

	<xsl:text>, [&#10;</xsl:text>

	<!-- Note: we should ignore declarations, annotations, etc. -->
	<!-- BUG: Actually, this ignoring should be done in the verbalizer,
	the XSLT should be a general purpose converter. -->
	<xsl:for-each select="owl11xml:*[local-name() != 'Declaration' and local-name() != 'Imports' and local-name() != 'Annotation']">

		<xsl:apply-templates select="."/>

		<xsl:if test="position() != last()">
			<xsl:text>,&#10;</xsl:text>
		</xsl:if>
	</xsl:for-each>

	<xsl:text>&#10;]).&#10;</xsl:text>
</xsl:template>

<!-- Note: we delete the prefix (until the hash). In general it is not a good idea,
as it removes the difference between variables from different namespaces. -->
<xsl:template match="owl11xml:OWLClass|owl11xml:ObjectProperty|owl11xml:DataProperty|owl11xml:Individual|owl11xml:Datatype">
	<xsl:choose>
		<xsl:when test="@URI = 'http://www.w3.org/2002/07/owl#Thing'">
			<xsl:text>'OWLClass'('owl:Thing')</xsl:text>
		</xsl:when>
		<xsl:when test="@URI = 'http://www.w3.org/2002/07/owl#Nothing'">
			<xsl:text>'OWLClass'('owl:Nothing')</xsl:text>
		</xsl:when>
		<xsl:otherwise>
			<xsl:text>'</xsl:text>
				<xsl:value-of select='local-name()'/>
			<xsl:text>'</xsl:text>
			<xsl:text>('</xsl:text>
				<xsl:choose>
					<xsl:when test="substring-after(@URI, '#') = ''">
						<xsl:value-of select='@URI'/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:value-of select="substring-after(@URI, '#')"/>
					</xsl:otherwise>
				</xsl:choose>
			<xsl:text>')</xsl:text>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>


<!-- Lists -->
<xsl:template match="owl11xml:EquivalentClasses|owl11xml:DisjointClasses|owl11xml:ObjectIntersectionOf|owl11xml:ObjectUnionOf|owl11xml:ObjectOneOf|owl11xml:SameIndividuals|owl11xml:DifferentIndividuals|owl11xml:SubObjectPropertyChain|owl11xml:DisjointObjectProperties|owl11xml:DataOneOf">
	<xsl:text>'</xsl:text>
	<xsl:value-of select="local-name()"/>
	<xsl:text>'([</xsl:text>

	<xsl:for-each select="owl11xml:*">

		<xsl:apply-templates select="."/>

		<xsl:if test="position() != last()">
			<xsl:text>, </xsl:text>
		</xsl:if>
	</xsl:for-each>
	<xsl:text>])</xsl:text>
</xsl:template>

<!-- Note: This rule is for numbers, we do not put them into quotes. -->
	<xsl:template match="owl11xml:Constant[@Datatype = 'http://www.w3.org/2001/XMLSchema#integer' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#double' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#int' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#short' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#long' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#nonNegativeInteger' or
		@Datatype = 'http://www.w3.org/2001/XMLSchema#float']"
		>
	<xsl:text>'^^'(</xsl:text>
	<xsl:value-of select='.'/>
	<xsl:text>, </xsl:text>
	<xsl:text>'</xsl:text>
	<xsl:value-of select="@Datatype"/>
	<xsl:text>'</xsl:text>
	<xsl:text>)</xsl:text>
</xsl:template>

<!-- Note: This rule is for everything else (mostly strings), we put them into quotes. -->
<xsl:template match="owl11xml:Constant">
	<xsl:text>'^^'(</xsl:text>
	<xsl:text>'</xsl:text>
	<xsl:value-of select='translate(., "&apos;", "`")'/>
	<xsl:text>'</xsl:text>
	<xsl:text>, </xsl:text>
	<xsl:text>'</xsl:text>
	<xsl:value-of select="@Datatype"/>
	<xsl:text>'</xsl:text>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="owl11xml:Annotation">
	<xsl:text>'Annotation'(</xsl:text>
	<xsl:text>'</xsl:text>
	<xsl:value-of select='translate(@annotationURI, "&apos;", "`")'/>
	<xsl:text>'</xsl:text>
	<xsl:text>, </xsl:text>
	<xsl:apply-templates/>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="owl11xml:*">
	<xsl:text>'</xsl:text>
	<xsl:value-of select="local-name()"/>
	<xsl:text>'(</xsl:text>

	<xsl:if test="@cardinality">
		<xsl:value-of select="@cardinality"/>
		<xsl:text>, </xsl:text>
	</xsl:if>

	<xsl:for-each select="owl11xml:*[local-name() != 'Comment']">

		<xsl:apply-templates select="."/>

		<xsl:if test="position() != last()">
			<xsl:text>, </xsl:text>
		</xsl:if>
	</xsl:for-each>
	<xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="text()"></xsl:template>

</xsl:stylesheet>
