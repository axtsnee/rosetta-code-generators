package com.regnosys.rosetta.generators.sample;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.regnosys.rosetta.generator.external.AbstractExternalGenerator;
import com.regnosys.rosetta.generator.java.RosettaJavaPackages;
import com.regnosys.rosetta.rosetta.RosettaClass;
import com.regnosys.rosetta.rosetta.RosettaRegularAttribute;
import com.regnosys.rosetta.rosetta.RosettaRootElement;

/**
 * Implementation of a code generator for the Groovy language
 */
public class SampleCodeGenerator extends AbstractExternalGenerator {

	/**
	 * Constructs a generator with a name
	 */
	public SampleCodeGenerator() {
		super("Sample");
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, ? extends CharSequence> generate(RosettaJavaPackages packages, List<RosettaRootElement> elements,
			String version) {
		Map<String, ? extends CharSequence> generatedFiles = elements.stream().filter(RosettaClass.class::isInstance)
				.map(RosettaClass.class::cast).collect(
						Collectors.toMap(e -> generateFilename(packages, e), e -> generateClass(packages, e, version)));

		return generatedFiles;
	}

	private String generateFilename(RosettaJavaPackages packages, RosettaClass clazz) {
		return packages.model().directoryName() + "/" + clazz.getName() + ".sample";
	}

	private String generateClass(RosettaJavaPackages packages, RosettaClass clazz, String version) {
		StringBuilder sb = new StringBuilder();
		sb.append("package ").append(packages.model().packageName());
		sb.append(GeneratorUtils.LINE_SEPARATOR).append(GeneratorUtils.LINE_SEPARATOR);
		sb.append(GeneratorUtils.groovyDocWithVersion(clazz.getDefinition(), version));
		sb.append("class ").append(clazz.getName()).append(" {");
		sb.append(GeneratorUtils.LINE_SEPARATOR);
		sb.append(attributes(clazz));
		sb.append("}");
		sb.append(GeneratorUtils.LINE_SEPARATOR);
		return sb.toString();
	}

	private static String attributes(RosettaClass clazz) {
		StringBuilder sb = new StringBuilder();
		for (RosettaRegularAttribute attr : clazz.getRegularAttributes()) {
			if (attr.getCard().getSup() == 1) {
				sb.append("\t").append(GeneratorUtils.toGroovyType(attr.getType().getName()) + " ")
						.append(attr.getName()).append(GeneratorUtils.LINE_SEPARATOR);
			} else if (attr.getCard().isIsMany()) {
				sb.append("\t").append("List<").append(GeneratorUtils.toGroovyType(attr.getType().getName()) + "> ")
						.append(attr.getName()).append(GeneratorUtils.LINE_SEPARATOR);
			}
		}
		return sb.toString();
	}

}
